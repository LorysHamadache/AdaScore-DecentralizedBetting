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


data BetType
instance Scripts.ValidatorTypes BetType where
    type instance DatumType BetType = BetDatum
    type instance RedeemerType BetType = BetRedeemer
  
-- VALIDATOR -- 

{-# INLINABLE mkValidator #-}
mkValidator :: BetDatum -> BetRedeemer -> ScriptContext -> Bool
mkValidator datum redeemer scontext = 
    traceIfFalse "Error: Invalid Script Input" (isScriptInputValid info datum)  -- Check that there is only 1 Script Input & that the Datum is fetchable & decodable
    && case redeemer of
            (BetRedeemerAccept _ _) -> mkValidatorAccept datum redeemer info
            (BetRedeemerOracle _ _) -> mkValidatorOracle datum redeemer info
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


-- No need to check signing
{-# INLINABLE mkValidatorAccept #-} -- To check: redeemer correct matchID
mkValidatorAccept :: BetDatum -> BetRedeemer -> TxInfo -> Bool
mkValidatorAccept datum redeemer info = 
    -- INPUT PART
    traceIfFalse "Error: Input    (Datum)    - Incorrect Status"              (AwaitingBet == d_status datum) &&
    traceIfFalse "Error: Input    (Datum)    - Incorrect Fee Calculation"     (d_fee datum == PlutusTx.Prelude.divide (d_amount datum * 5) 100 + 2000000) &&
    traceIfFalse "Error: Input    (Redeemer) - MatchID Incorrect"             (r_matchID redeemer == d_matchID datum) &&
    traceIfFalse "Error: Input    (Redeemer) - Creator Incorrect"             (r_creator redeemer == d_creator datum) &&
    traceIfFalse "Error: Input    (Time)     - Betting Window Closed"         (after (d_closedAt datum) (txInfoValidRange info)) &&
    traceIfFalse "Error: Input    (Script)   - Invalid Value"                 ((getScriptInputValue $ txInfoInputs info) == amountScriptInput) &&
    traceIfFalse "Error: Input    (Acceptor) - Invalid Value"                 (getInputAcceptorValue (txInfoInputs info) >= acceptorInput) &&
    traceIfFalse "Error: Output   (Datum)    - Incorrect Datum"               (output_datum{d_acceptor = d_acceptor datum} == datum{d_status = AwaitingResult}) &&
    traceIfFalse "Error: Output   (Script)   - Invalid Value"                 (output_value == acceptorInput)
    where
        amountScriptInput = d_fee datum + d_amount datum
        acceptorInput = (PlutusTx.Prelude.divide ((d_amount datum) * (d_odds datum)) 100)+1+ (d_fee datum)
        output_datum = getScriptOutputDatum info
        output_value = (Ada.getLovelace . Ada.fromValue) $ txOutValue $ getScriptOutput $ txInfoOutputs info


{-# INLINABLE mkValidatorOracle #-}
mkValidatorOracle :: BetDatum -> BetRedeemer -> TxInfo -> Bool
mkValidatorOracle datum redeemer info = 
    traceIfFalse "Error: Input (Datum) - Incorrect Status"              (AwaitingResult == d_status datum)






    -- traceIfFalse "Hello" False
    
    -- traceIfFalse (In.appendString "Hello"  (In.decodeUtf8 $ In.unsafeDataAsB $ PlutusTx.toBuiltinData $ redeemer)) False


tValidator :: Scripts.TypedValidator BetType
tValidator = Scripts.mkTypedValidator @BetType 
          $$(PlutusTx.compile [|| mkValidator ||])
          $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @BetDatum @BetRedeemer

validator :: Validator
validator = Scripts.validatorScript tValidator

scrAddress :: Address
scrAddress = scriptAddress validator    











