{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
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


module Traces.NormalBehaviorTrace where

import           Types
import           Utils
import           TxConstruction
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
import           Prelude                (IO, show, String)
import           Control.Monad.Freer.Extras           as Extras
import           Control.Monad          hiding (fmap)
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger.TimeSlot
import           Plutus.Contract.Test
import qualified Ledger.Ada   as Ada
import           Control.Lens
import           Test.Tasty
import           Data.Monoid                  (Last (..))



trace_normal_behavior:: MatchBet -> EmulatorTrace ()
trace_normal_behavior bres = do
    ------- INITIALIZATION
    better1_wallet <- activateContractWallet (knownWallet 1) endpoints
    acceptor1_wallet <- activateContractWallet (knownWallet 2) endpoints
    oracle_wallet <- activateContractWallet (knownWallet 5) endpoints
    
    ------- EXECUTION
    callEndpoint @"create" better1_wallet $ CreateParams {
        create_matchID     = "48656c6c6f204c6f727973",
        create_closedAt    = 10_000,
        create_resultAt    = 50_000,
        create_creatorbet  = Win,
        create_odds        = 150,
        create_amount      = 50_000_000
        }
    t <- Emulator.waitUntilTime 3000

    Last creation_scriptTxId <- observableState better1_wallet
    case creation_scriptTxId of 
        Nothing -> Extras.logError @String "No Script TxOut TxId"
        Just x -> do
            callEndpoint @"accept" acceptor1_wallet (x, AcceptParams {
                        accept_matchID     = "48656c6c6f204c6f727973",
                        accept_creator     = mockWalletPaymentPubKeyHash (knownWallet 1)
                    })

    void $ Emulator.waitUntilTime 15_000     

    callEndpoint @"oracle" oracle_wallet $ OracleParams{
        oracle_matchID = "48656c6c6f204c6f727973",
        oracle_result = bres,
        oracle_end = 100_000
    }

    void $ Emulator.waitUntilTime 25_000

    Last accept_scriptTxId <- observableState acceptor1_wallet
    Last oracle_scriptTxId <- observableState oracle_wallet
    case accept_scriptTxId of 
        Nothing -> Extras.logError @String "No Script TxOut TxId"
        Just scpinput -> case oracle_scriptTxId of
            Nothing -> Extras.logError @String "No Oracle Reference Input"
            Just oclref -> do
                if (bres == Win) then do
                    better1_wallet <- activateContractWallet (knownWallet 1) endpoints
                    callEndpoint @"close" better1_wallet (scpinput,oclref, CloseParams{
                        close_matchID     = "48656c6c6f204c6f727973"
                    }) 
                else do
                    acceptor1_wallet <- activateContractWallet (knownWallet 2) endpoints
                    callEndpoint @"close" acceptor1_wallet (scpinput,oclref, CloseParams{
                        close_matchID     = "48656c6c6f204c6f727973"
                    }) 
    
    s <- Emulator.waitNSlots 1

    Extras.logInfo $ "End of Simulation at slot " ++ show s

