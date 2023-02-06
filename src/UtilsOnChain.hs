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

module UtilsOnChain where

import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Plutus.V2.Ledger.Api
import           Ledger.Address
import qualified Ledger.Ada  as Ada    


{-# INLINABLE service_percfee #-}
service_percfee :: Integer
service_percfee = 5

{-# INLINABLE service_minfee #-}
service_minfee :: Integer
service_minfee = 2_000_000

{-# INLINABLE bet_minamount #-}
bet_minamount :: Integer
bet_minamount = 5_000_000

-- ON CHAIN HELPER FUNCTIONS

{-# INLINABLE getFeeCalculation #-}
getFeeCalculation :: Integer -> Integer
getFeeCalculation amount = max percent service_minfee
    where 
        percent = PlutusTx.Prelude.divide (amount* service_percfee) 100

{-# INLINABLE getInputValue #-}
getInputValue :: [TxInInfo] -> Integer
getInputValue i =  sum input_list
    where 
        input_list = map (Ada.getLovelace . Ada.fromValue . txOutValue . txInInfoResolved) i

{-# INLINABLE getTxValueAt #-}
getTxValueAt :: PaymentPubKeyHash -> [TxOut] -> Integer
getTxValueAt pkh list = sum $ map (Ada.getLovelace . Ada.fromValue . txOutValue) txs
    where 
        txs = filter (\x -> txOutAddress x == pubKeyHashAddress pkh Nothing) list
