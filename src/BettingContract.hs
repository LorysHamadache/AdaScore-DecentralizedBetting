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
import           Utils
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Prelude                (Show,show)
import           Text.Printf            (printf)
import qualified PlutusTx.Builtins.Internal as In
import           PlutusTx.Builtins
import qualified Plutus.V1.Ledger.Ada   as Ada
import           Ledger.Constraints     as Constraints
import           Ledger.Bytes


data BetType
instance Scripts.ValidatorTypes BetType where
    type instance DatumType BetType = BetDatum
    type instance RedeemerType BetType = BetRedeemer
  
-- VALIDATOR -- 

{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> BetDatum -> BetRedeemer -> ScriptContext -> Bool
mkValidator oracle_pkh' datum redeemer scontext = 
    traceIfFalse  "Error: Input    (Script)   - Script Input Invalid"         (isScriptInputValid info datum) && -- Check that there is only 1 Script Input & that the Datum is fetchable & decodable
    traceIfFalse  "Error: Input    (Datum)    - Incorrect Fee Calculation"    (d_fee datum == (getFeeCalculation $ d_amount datum)) &&
    traceIfFalse  "Error: Input    (Redeemer) - MatchID Incorrect"            (r_matchID redeemer == d_matchID datum) &&
    traceIfFalse  "Error: Input    (Datum)    - Creator Bet Incorrect"        (d_creatorbet datum /= Unknown) &&
    traceIfFalse  "Error: Input    (Datum)    - Incorrect Odds"               ((d_odds datum)> 100 && (d_odds datum) <= 1000) &&
    traceIfFalse  "Error: Input    (Datum)    - Amount too low"               (d_amount datum >=  bet_minamount) &&
    traceIfFalse  "Error: Input    (Datum)    - Incorrect Result Limit"       (d_resultlimAt datum  > d_closedAt datum) &&
    traceIfFalse  "Error: Input    (Datum)    - Result field incorrect"       (d_result datum == Unknown) &&
    case redeemer of
            (BetRedeemerAccept _ _) -> mkValidatorAccept datum redeemer info
            (BetRedeemerOracle _ _) -> mkValidatorOracle oracle_pkh' datum redeemer info
            (BetRedeemerClose _) -> mkValidatorClose datum redeemer info
    where
        info :: TxInfo
        info = scriptContextTxInfo scontext
        
-- No need to check signing
{-# INLINABLE mkValidatorAccept #-}
mkValidatorAccept :: BetDatum -> BetRedeemer -> TxInfo -> Bool
mkValidatorAccept datum redeemer info = 
    traceIfFalse "Error: Input    (Datum)    - Incorrect Status"              (AwaitingBet == d_status datum) &&
    traceIfFalse "Error: Input    (Redeemer) - Creator Incorrect"             (r_creator redeemer == d_creator datum) &&
    traceIfFalse "Error: Input    (Time)     - Betting Window Closed"         (after (d_closedAt datum) (txInfoValidRange info)) &&
    traceIfFalse "Error: Input    (Script)   - Invalid Value"                 ((getScriptInputValue $ txInfoInputs info) == amountScriptInput) &&
    traceIfFalse "Error: Input    (Acceptor) - Invalid Value"                 (getInputValue (txInfoInputs info) >= expected_input_value - amountScriptInput) &&
    traceIfFalse "Error: Output   (Datum)    - Incorrect Datum"               (output_datum{d_acceptor = d_acceptor datum} == datum{d_status = AwaitingResult}) &&
    traceIfFalse "Error: Output   (Script)   - Invalid Value"                 (output_value == expected_input_value)
    where
        amountScriptInput = d_fee datum + d_amount datum
        expected_input_value = (PlutusTx.Prelude.divide ((d_amount datum) * (d_odds datum)) 100)+ (d_fee datum)
        output_datum = getScriptOutputDatum info -- Technically Check if the Output Datum is Decodable & If only 1 Script
        output_value = (Ada.getLovelace . Ada.fromValue) $ txOutValue $ getScriptOutput $ txInfoOutputs info



{-# INLINABLE mkValidatorOracle #-}
mkValidatorOracle :: PubKeyHash -> BetDatum -> BetRedeemer -> TxInfo -> Bool
mkValidatorOracle oracle_pkh' datum redeemer info = --True --traceError $ In.decodeUtf8 $ getPubKeyHash $ head $ (txInfoSignatories info)
    traceIfFalse "Error: Input (Datum)    - Incorrect Status"              (AwaitingResult == d_status datum) &&
    traceIfFalse "Error: Input (Oracle)   - Tx not signed by Oracle"       (length (filter (\x ->  getPubKeyHash x == getPubKeyHash oracle_pkh') (txInfoSignatories info)) >= 1) &&
    traceIfFalse "Error: Input (Script)   - Script Value Incorrect"        (expected_input_value == (getScriptInputValue $ txInfoInputs info)) &&
    traceIfFalse "Error: Input (Redeemer) - Incorrect Result"              (r_result redeemer /= Unknown) &&
    traceIfFalse "Error: Input (Time)     - Betting Window must be closed" (before (d_closedAt datum) (txInfoValidRange info)) &&
    traceIfFalse "Error: Input (Time)     - Result Window closed"          (after (d_resultlimAt datum) (txInfoValidRange info)) &&
    traceIfFalse "Error: Output (Oracle)  - Incorrect Service Fee Payment" (getTxValueAt (PaymentPubKeyHash oracle_pkh') (txInfoOutputs info) ==
                                                                            getTxValueAt (PaymentPubKeyHash oracle_pkh') (map txInInfoResolved  (txInfoInputs info)) + (d_fee datum) - bctransaction_fee) &&
    traceIfFalse "Error: Output (Winner)  - Incorrect Winner Payment"      (getTxValueAt winner_ppkh (txInfoOutputs info) == 
                                                                            getTxValueAt winner_ppkh (map txInInfoResolved  (txInfoInputs info)) + bet_value)                                                                        

    where 
        bet_value = (PlutusTx.Prelude.divide ((d_amount datum) * (d_odds datum)) 100)
        expected_input_value = bet_value+ (d_fee datum)
        winner_ppkh = if (r_result redeemer == d_creatorbet datum) then d_creator datum else d_acceptor datum
        bctransaction_fee = Ada.getLovelace . Ada.fromValue $ txInfoFee info



{-# INLINABLE mkValidatorClose #-}
mkValidatorClose :: BetDatum -> BetRedeemer -> TxInfo -> Bool
mkValidatorClose datum redeemer info = 
    if (d_status datum == AwaitingBet) then
        traceIfFalse "Error: Input (Script)   - Script Value Incorrect"        (expected_input_value_ab == (getScriptInputValue $ txInfoInputs info)) &&
        traceIfFalse "Error: Input (Closer)   - Tx not signed by Creator"      (length (filter (\x ->  getPubKeyHash x == (getPubKeyHash $ unPaymentPubKeyHash $ (d_creator datum))) (txInfoSignatories info)) >= 1) &&
        traceIfFalse "Error: Output (Creator) - Incorrect Refund Creator"      (getTxValueAt (d_creator datum) (txInfoOutputs info) == 
                                                                                    getTxValueAt (d_creator datum) (map txInInfoResolved  (txInfoInputs info)) + expected_input_value_ab-bctransaction_fee)
    else if (d_status datum == AwaitingResult) then
            traceIfFalse "Error: Input (Script)   - Script Value Incorrect"        (expected_input_value_ar == (getScriptInputValue $ txInfoInputs info)) &&
            traceIfFalse "Error: Input (Closer)   - Tx not signed by Creator"      ((length (filter (\x ->  getPubKeyHash x == (getPubKeyHash $ unPaymentPubKeyHash $ (d_creator datum))) (txInfoSignatories info)) >= 1) ||
                                                                                   (length (filter (\x ->  getPubKeyHash x == (getPubKeyHash $ unPaymentPubKeyHash $ (d_acceptor datum))) (txInfoSignatories info)) >= 1)) &&
            traceIfFalse "Error: Output (Creator) - Incorrect Refund Creator"      (creator_refund_diff >= 0 && creator_refund_diff <= bctransaction_fee) &&
            traceIfFalse "Error: Output (Acceptor) - Incorrect Refund Acceptor"    (acceptor_refund_diff >= 0 && acceptor_refund_diff <= bctransaction_fee) &&
            traceIfFalse "Error: Input (Time)     - Close Window not opened yet"   (before (d_resultlimAt datum) (txInfoValidRange info))
    else False
                                
    where 
        expected_input_value_ab = (d_amount datum) + (d_fee datum)
        expected_input_value_ar = (PlutusTx.Prelude.divide ((d_amount datum) * (d_odds datum)) 100)+ (d_fee datum)
        bctransaction_fee = Ada.getLovelace . Ada.fromValue $ txInfoFee info
        creator_refund_diff = (getTxValueAt (d_creator datum) (map txInInfoResolved  (txInfoInputs info)) + expected_input_value_ab) - (getTxValueAt (d_creator datum) (txInfoOutputs info))
        acceptor_refund_diff = (getTxValueAt (d_acceptor datum) (map txInInfoResolved  (txInfoInputs info)) + expected_input_value_ar - expected_input_value_ab) - (getTxValueAt (d_acceptor datum) (txInfoOutputs info))


tValidator :: Scripts.TypedValidator BetType
tValidator = Scripts.mkTypedValidator @BetType 
          ($$(PlutusTx.compile [|| mkValidator ||])
            `PlutusTx.applyCode` PlutusTx.liftCode oracle_pkh)
          $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @BetDatum @BetRedeemer

validator :: Validator
validator = Scripts.validatorScript tValidator

scrAddress :: Address
scrAddress = scriptAddress validator    











