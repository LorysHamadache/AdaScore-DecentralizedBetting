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
    traceIfFalse "Error: Input    (Script)   - Script Input Invalid"         (isScriptInputValid info datum) && -- Check that there is only 1 Script Input & that the Datum is fetchable & decodable
    traceIfFalse "Error: Input    (Datum)    - Incorrect Fee Calculation"    (d_fee datum == PlutusTx.Prelude.divide (d_amount datum * 5) 100 + 2_000_000) &&
    traceIfFalse "Error: Input    (Redeemer) - MatchID Incorrect"            (r_matchID redeemer == d_matchID datum) &&
    traceIfFalse "Error: Input    (Datum)    - Creator Bet Incorrect"        (d_creatorbet datum /= Unknown) &&
    case redeemer of
            (BetRedeemerAccept _ _) -> mkValidatorAccept datum redeemer info
            (BetRedeemerOracle _ _) -> mkValidatorOracle oracle_pkh' datum redeemer info
    where
        info :: TxInfo
        info = scriptContextTxInfo scontext
        

--d_matchID     = create_matchID param,
--d_closedAt    = create_closedAt param,
--d_resultAt    = create_resultAt param,
--d_result      = Unknown,
--d_creatorbet  = create_creatorbet param ,
--d_odds        = create_odds param,
--d_amount      = create_amount param ,
--d_fee         = PlutusTx.Prelude.divide (create_amount param * 5) 100 + 2000000,
--d_creator     = pkh,
--d_acceptor    = pkh,
--d_status      = AwaitingBet

--r_matchID     :: Builtins.BuiltinByteString,
--r_result      :: MatchBet
 
-- No need to check signing
{-# INLINABLE mkValidatorAccept #-}
mkValidatorAccept :: BetDatum -> BetRedeemer -> TxInfo -> Bool
mkValidatorAccept datum redeemer info = 
    traceIfFalse "Error: Input    (Datum)    - Incorrect Status"              (AwaitingBet == d_status datum && d_result datum == Unknown) &&
    traceIfFalse "Error: Input    (Redeemer) - Creator Incorrect"             (r_creator redeemer == d_creator datum) &&
    traceIfFalse "Error: Input    (Time)     - Betting Window Closed"         (after (d_closedAt datum) (txInfoValidRange info)) &&
    traceIfFalse "Error: Input    (Script)   - Invalid Value"                 ((getScriptInputValue $ txInfoInputs info) == amountScriptInput) &&
    traceIfFalse "Error: Input    (Acceptor) - Invalid Value"                 (getInputValue (txInfoInputs info) >= expected_input_value) &&
    traceIfFalse "Error: Output   (Datum)    - Incorrect Datum"               (output_datum{d_acceptor = d_acceptor datum} == datum{d_status = AwaitingResult}) &&
    traceIfFalse "Error: Output   (Script)   - Invalid Value"                 (output_value == expected_input_value)
    where
        amountScriptInput = d_fee datum + d_amount datum
        expected_input_value = (PlutusTx.Prelude.divide ((d_amount datum) * (d_odds datum)) 100)+ (d_fee datum)
        output_datum = getScriptOutputDatum info
        output_value = (Ada.getLovelace . Ada.fromValue) $ txOutValue $ getScriptOutput $ txInfoOutputs info



{-# INLINABLE mkValidatorOracle #-}
mkValidatorOracle :: PubKeyHash -> BetDatum -> BetRedeemer -> TxInfo -> Bool
mkValidatorOracle oracle_pkh' datum redeemer info = --True --traceError $ In.decodeUtf8 $ getPubKeyHash $ head $ (txInfoSignatories info)
    traceIfFalse "Error: Input (Datum)   - Incorrect Status"              (AwaitingResult == d_status datum) &&
    traceIfFalse "Error: Input (Oracle)  - Tx not signed by Oracle"       (length (filter (\x ->  getPubKeyHash x == getPubKeyHash oracle_pkh') (txInfoSignatories info)) >= 1)



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











