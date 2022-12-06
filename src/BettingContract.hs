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
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import qualified Ledger.Typed.Scripts   as Scripts


data BetType
instance Scripts.ValidatorTypes BetType where
    type instance DatumType BetType = BetDatum
    type instance RedeemerType BetType = BetReedemer
  
-- VALIDATOR -- 

{-# INLINABLE mkValidator #-}
mkValidator :: BetDatum -> BetReedemer -> ScriptContext -> Bool
mkValidator datum reedemer scontext = True


tValidator :: Scripts.TypedValidator BetType
tValidator = Scripts.mkTypedValidator @BetType 
          $$(PlutusTx.compile [|| mkValidator ||])
          $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @BetDatum @BetReedemer

validator :: Validator
validator = Scripts.validatorScript tValidator

scrAddress :: Address
scrAddress = scriptAddress validator    











