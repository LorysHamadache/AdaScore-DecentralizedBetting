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
    (length scriptinputs == 1) && -- Added
    (d_fee datum == (getFeeCalculation $ d_amount datum)) && --Def not needed
    (r_matchID redeemer == d_matchID datum) && -- Added
    (d_creatorbet datum /= Unknown) && -- Not needed as incentive is to put the right element
    ((d_odds datum)> 100 && (d_odds datum) <= 1000) && -- Added 
    (d_amount datum >=  bet_minamount) && -- Added 
    (d_resultlimAt datum  > d_closedAt datum) && -- Unsure if necessary. We are checking the datum by the better, but he has incentive to have the right limit date
    (d_result datum == Unknown) && -- Doesnt matter, the result is taken from the oracle, it is just for reporting purposes at the end
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
    (AwaitingBet == d_status datum) && -- OK
    (r_creator redeemer == d_creator datum) && -- useless cause the d_creator is taken to make the transaction
    (after (d_closedAt datum) (txInfoValidRange info)) && -- OK
    ((Ada.getLovelace $ Ada.fromValue $ txOutValue scinput) == better_amount) && -- OK
    (getInputValue (txInfoInputs info) >= total_amount) && -- OK
    (length output == 1) && -- OK
    (output_datum{d_acceptor = d_acceptor datum} == datum{d_status = AwaitingResult}) && --OK
    ((Ada.getLovelace $ Ada.fromValue $ txOutValue (head output))== total_amount) -- OK
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
        (better_amount == (Ada.getLovelace $ Ada.fromValue $ txOutValue scinput)) && -- OK
        (length (filter (\x ->  getPubKeyHash x == (getPubKeyHash $ unPaymentPubKeyHash $ (d_creator datum))) (txInfoSignatories info)) >= 1) && --OK
        (getTxValueAt (d_creator datum) (txInfoOutputs info) == getTxValueAt (d_creator datum) (map txInInfoResolved  (txInfoInputs info)) + better_amount - cardano_fee) --OK
    else if (d_status datum == AwaitingResult) then
        (total_amount == (Ada.getLovelace $ Ada.fromValue $ txOutValue scinput)) && --OK
        ((length (filter (\x ->  getPubKeyHash x == (getPubKeyHash $ unPaymentPubKeyHash $ (d_creator datum))) (txInfoSignatories info)) >= 1) || -- OK
        (length (filter (\x ->  getPubKeyHash x == (getPubKeyHash $ unPaymentPubKeyHash $ (d_acceptor datum))) (txInfoSignatories info)) >= 1)) && --OK
        (creator_refund_diff >= 0 && acceptor_refund_diff >= 0 && creator_refund_diff <= cardano_fee && acceptor_refund_diff <= cardano_fee) &&
        (abs(creator_refund_diff - acceptor_refund_diff) == cardano_fee) &&
        (before (d_resultlimAt datum) (txInfoValidRange info))
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
    (AwaitingResult == d_status datum) && -- OK
    (length ref_inputs_list == 1) && -- OK
    (txOutAddress ref_input == pubKeyHashAddress (oracle_pkh) Nothing) && --OK
    (total_amount == (Ada.getLovelace $ Ada.fromValue $ txOutValue scinput)) && -- OK
    ((o_result oracle_datum /= Unknown) && (o_matchID oracle_datum == d_matchID datum)) && -- OK
    (before (d_closedAt datum) (txInfoValidRange info)) && --Inutile, pas la reference de dispo
    ((after (d_resultlimAt datum) (txInfoValidRange info)) && (after (o_end oracle_datum) (txInfoValidRange info))) && --sensé etre egal OK
    (getTxValueAt (oracle_pkh) (txInfoOutputs info) == getTxValueAt (oracle_pkh) (map txInInfoResolved  (txInfoInputs info)) + (d_fee datum)) && 
    (getTxValueAt winner_ppkh (txInfoOutputs info) == getTxValueAt winner_ppkh (map txInInfoResolved  (txInfoInputs info)) + total_amount - (d_fee datum) - cardano_fee)  &&
    (length (filter (\x ->  getPubKeyHash x == (getPubKeyHash $ unPaymentPubKeyHash $ winner_ppkh)) (txInfoSignatories info)) >= 1) 
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



 







