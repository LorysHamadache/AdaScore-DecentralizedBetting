{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NumericUnderscores  #-}

module BettingContract where

import           Types
import           UtilsOnChain
import           PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Plutus.V2.Ledger.Api
import           Ledger.Address
import qualified Ledger.Ada  as Ada
import           Plutus.V2.Ledger.Tx
import           Plutus.V1.Ledger.Interval

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator datumData redeemerData scontextData =
    if (
    traceIfFalse  "Error: Input    (Script)   - Script Input Invalid"      (length scriptinputs == 1) &&
    traceIfFalse  "Error: Input    (Datum)    - Incorrect Fee Calculation" (d_fee datum == (getFeeCalculation $ d_amount datum)) &&
    traceIfFalse  "Error: Input    (Redeemer) - MatchID Incorrect"         (r_matchID redeemer == d_matchID datum) &&
    traceIfFalse  "Error: Input    (Datum)    - Creator Bet Incorrect"     (d_creatorbet datum /= Unknown) &&
    traceIfFalse  "Error: Input    (Datum)    - Incorrect Odds"            ((d_odds datum)> 100 && (d_odds datum) <= 1000) &&
    traceIfFalse  "Error: Input    (Datum)    - Amount too low"            (d_amount datum >=  bet_minamount) &&
    traceIfFalse  "Error: Input    (Datum)    - Incorrect Result Limit"    (d_resultlimAt datum  > d_closedAt datum) &&
    traceIfFalse  "Error: Input    (Datum)    - Result field incorrect"    (d_result datum == Unknown) &&
    validate_action (r_action redeemer)
    )
    then ()
    else error ()
    where
        datum::BetDatum
        datum = unsafeFromBuiltinData datumData
        redeemer::BetRedeemer
        redeemer = unsafeFromBuiltinData redeemerData
        scontext::ScriptContext
        scontext = unsafeFromBuiltinData scontextData
        scriptinputs = filter (isPayToScriptOut) (map txInInfoResolved (txInfoInputs info))
        tx =  (head scriptinputs)
        info = scriptContextTxInfo scontext
        validate_action::BuiltinByteString -> Bool
        validate_action action
            | action == "accept" = mkValidatorAccept datum redeemer info tx
            | action == "cancel" = mkValidatorCancel datum redeemer info tx
            | action == "close" = mkValidatorClose datum redeemer info tx
            | otherwise = False


{-# INLINABLE mkValidatorAccept #-}
mkValidatorAccept :: BetDatum -> BetRedeemer -> TxInfo -> TxOut -> Bool
mkValidatorAccept datum redeemer info scinput= 
    traceIfFalse "Error: Input  (Datum)    - Incorrect Status"      (AwaitingBet == d_status datum) &&
    traceIfFalse "Error: Input  (Redeemer) - Creator Incorrect"     (r_creator redeemer == d_creator datum) && 
    traceIfFalse "Error: Input  (Time)     - Betting Window Closed" (after (d_closedAt datum) (txInfoValidRange info)) &&
    traceIfFalse "Error: Input  (Script)   - Invalid Value"         ((Ada.getLovelace $ Ada.fromValue $ txOutValue scinput) == better_amount) &&
    traceIfFalse "Error: Input  (Acceptor) - Invalid Value"         (getInputValue (txInfoInputs info) >= total_amount) &&
    traceIfFalse "Error: Output (Script)   - Only 1 Script Output"  (length output == 1) &&
    traceIfFalse "Error: Output (Datum)    - Incorrect Datum"       (output_datum{d_acceptor = d_acceptor datum} == datum{d_status = AwaitingResult}) &&
    traceIfFalse "Error: Output (Script)   - Invalid Value"         ((Ada.getLovelace $ Ada.fromValue $ txOutValue (head output))== total_amount)
    where
        better_amount = d_fee datum + d_amount datum
        total_amount  = (PlutusTx.Prelude.divide ((d_amount datum) * (d_odds datum)) 100)+ (d_fee datum)
        output = filter (isPayToScriptOut)  (txInfoOutputs  info)
        output_datum::BetDatum
        output_datum = case txOutDatum (head output) of
            OutputDatum (Datum d) -> PlutusTx.unsafeFromBuiltinData d
            _ -> error ()


{-# INLINABLE mkValidatorCancel #-}
mkValidatorCancel :: BetDatum -> BetRedeemer -> TxInfo -> TxOut -> Bool
mkValidatorCancel datum redeemer info scinput = 
    if (d_status datum == AwaitingBet) then
        traceIfFalse "Error: Input  (Script)    - Script Value Incorrect"        (better_amount == (Ada.getLovelace $ Ada.fromValue $ txOutValue scinput)) &&
        traceIfFalse "Error: Input  (Cancellor) - Tx not signed by Creator"      (length (filter (\x ->  getPubKeyHash x == (getPubKeyHash $ unPaymentPubKeyHash $ (d_creator datum))) (txInfoSignatories info)) >= 1) && 
        traceIfFalse "Error: Output (Creator)   - Incorrect Refund Creator"      (getTxValueAt (d_creator datum) (txInfoOutputs info) == 
                                                                                    getTxValueAt (d_creator datum) (map txInInfoResolved  (txInfoInputs info)) + better_amount - cardano_fee)
    else if (d_status datum == AwaitingResult) then
        traceIfFalse "Error: Input (Script)   - Script Value Incorrect"          (total_amount == (Ada.getLovelace $ Ada.fromValue $ txOutValue scinput)) &&
        traceIfFalse "Error: Input (Closer)   - Tx not signed by Creator"        ((length (filter (\x ->  getPubKeyHash x == (getPubKeyHash $ unPaymentPubKeyHash $ (d_creator datum))) (txInfoSignatories info)) >= 1) ||
                                                                                   (length (filter (\x ->  getPubKeyHash x == (getPubKeyHash $ unPaymentPubKeyHash $ (d_acceptor datum))) (txInfoSignatories info)) >= 1)) &&
        traceIfFalse "Error: Output (C&A) - Incorrect Refund"                    (creator_refund_diff >= 0 && acceptor_refund_diff >= 0 && creator_refund_diff <= cardano_fee && acceptor_refund_diff <= cardano_fee) &&
        traceIfFalse "Error: Output (C&A) - Incorrect Refund"                    (abs(creator_refund_diff - acceptor_refund_diff) == cardano_fee) &&
        traceIfFalse "Error: Input (Time)     - Close Window not opened yet"     (before (d_resultlimAt datum) (txInfoValidRange info))
    else False
    where 
        better_amount = d_fee datum + d_amount datum
        cardano_fee = Ada.getLovelace . Ada.fromValue $ txInfoFee info
        total_amount = (PlutusTx.Prelude.divide ((d_amount datum) * (d_odds datum)) 100)+ (d_fee datum)
        creator_refund_diff =  (getTxValueAt (d_creator datum) (map txInInfoResolved  (txInfoInputs info))) - (getTxValueAt (d_creator datum) (txInfoOutputs info)) + better_amount
        acceptor_refund_diff = (getTxValueAt (d_acceptor datum) (map txInInfoResolved  (txInfoInputs info))) - (getTxValueAt (d_acceptor datum) (txInfoOutputs info)) + total_amount - better_amount


{-# INLINABLE mkValidatorClose #-}
mkValidatorClose :: BetDatum -> BetRedeemer -> TxInfo -> TxOut  -> Bool
mkValidatorClose datum redeemer info scinput = 
    traceIfFalse "Error: Input (Datum)    - Incorrect Status"               (AwaitingResult == d_status datum) &&
    traceIfFalse "Error: Input (Oracle)   - Must Have 1 Reference Input"    (length ref_inputs_list == 1) && 
    traceIfFalse "Error: Input (Oracle)   - Must Come from Official Oracle" (txOutAddress ref_input == pubKeyHashAddress (oracle_pkh) Nothing) &&
    traceIfFalse "Error: Input (Script)   - Script Value Incorrect"         (total_amount == (Ada.getLovelace $ Ada.fromValue $ txOutValue scinput)) &&
    traceIfFalse "Error: Input (Oracle)   - Incorrect Result"               ((o_result oracle_datum /= Unknown) && (o_matchID oracle_datum == d_matchID datum)) &&
    traceIfFalse "Error: Input (Time)     - Betting Window must be closed"  (before (d_closedAt datum) (txInfoValidRange info)) &&
    traceIfFalse "Error: Input (Time)     - Result Window closed"           ((after (d_resultlimAt datum) (txInfoValidRange info)) && (after (o_end oracle_datum) (txInfoValidRange info))) &&
    traceIfFalse "Error: Output (Oracle)  - Incorrect Service Fee Payment" (getTxValueAt (oracle_pkh) (txInfoOutputs info) ==
                                                                            getTxValueAt (oracle_pkh) (map txInInfoResolved  (txInfoInputs info)) + (d_fee datum)) &&
    traceIfFalse "Error: Output (Winner)  - Incorrect Winner Payment"      (getTxValueAt winner_ppkh (txInfoOutputs info) == 
                                                                            getTxValueAt winner_ppkh (map txInInfoResolved  (txInfoInputs info)) + total_amount - (d_fee datum) - cardano_fee)  &&
    traceIfFalse "Error: Input  (Winner)   - Tx not signed by Winner"      (length (filter (\x ->  getPubKeyHash x == (getPubKeyHash $ unPaymentPubKeyHash $ winner_ppkh)) (txInfoSignatories info)) >= 1) 
    where
        ref_inputs_list = (map txInInfoResolved (txInfoReferenceInputs info))
        ref_input = head ref_inputs_list
        cardano_fee = Ada.getLovelace . Ada.fromValue $ txInfoFee info
        total_amount = (PlutusTx.Prelude.divide ((d_amount datum) * (d_odds datum)) 100) + (d_fee datum)
        oracle_datum::BetDatum
        oracle_datum = case txOutDatum ref_input of
            OutputDatum (Datum d) -> PlutusTx.unsafeFromBuiltinData d
            _ -> error ()
        winner_ppkh = if (o_result oracle_datum == d_creatorbet datum) then d_creator datum else d_acceptor datum
        oracle_pkh :: PaymentPubKeyHash
        oracle_pkh = PaymentPubKeyHash $ PubKeyHash { getPubKeyHash = (PlutusTx.Prelude.foldr (\x y -> consByteString x y) emptyByteString [191,52,45,221,59,26,97,145,212,206,147,108,146,210,152,52,214,135,158,223,40,73,234,234,132,200,39,248]) }



validator :: Validator
validator = mkValidatorScript
          $$(PlutusTx.compile [|| mkValidator ||])



 







