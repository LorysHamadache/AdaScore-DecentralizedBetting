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
        



{-# INLINABLE mkValidatorAccept #-} -- To check: redeemer correct matchID
mkValidatorAccept :: BetDatum -> BetRedeemer -> TxInfo -> Bool
mkValidatorAccept datum redeemer info = 
    traceIfFalse "Error: Redeemer - MatchID Incorrect" (r_matchID redeemer == d_matchID datum) &&
    traceIfFalse "Error: Redeemer - Creator Incorrect" (r_creator redeemer == d_creator datum) &&
    traceIfFalse "Error: Time - Betting Window Closed" (after (d_closedAt datum) (txInfoValidRange info))


{-# INLINABLE mkValidatorOracle #-}
mkValidatorOracle :: BetDatum -> BetRedeemer -> TxInfo -> Bool
mkValidatorOracle datum redeemer info = True






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











