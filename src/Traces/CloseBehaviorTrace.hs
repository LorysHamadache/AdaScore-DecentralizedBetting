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


module Traces.CloseBehaviorTrace where

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

trace_close_nobet_behavior:: EmulatorTrace ()
trace_close_nobet_behavior = do
    ------- INITIALIZATION
    better1_wallet <- activateContractWallet (knownWallet 1) endpoints
    acceptor1_wallet <- activateContractWallet (knownWallet 2) endpoints
    s_conf <- getSlotConfig
    
    ------- EXECUTION
    callEndpoint @"create" better1_wallet $ CreateParams {
        create_matchID     = "48656c6c6f204c6f727973",
        create_closedAt    = 10_000,
        create_resultAt    = 50_000,
        create_creatorbet  = Win,
        create_odds        = 150,
        create_amount      = 50_000_000
        }

    void $ Emulator.waitUntilTime 3_000

    Last creation_scriptTxId <- observableState better1_wallet
    case creation_scriptTxId of 
        Nothing -> Extras.logError @String "No Script TxOut TxId"
        Just x -> do
            Extras.logInfo @String "Calling  End"
            better1_wallet <- activateContractWallet (knownWallet 1) endpoints
            callEndpoint @"cancel" better1_wallet (x, CloseParams{
                        close_matchID     = "48656c6c6f204c6f727973"
                    })

    s <- Emulator.waitNSlots 1
    Extras.logInfo $ "End of Simulation at slot " ++ show s

trace_close_nooracle_behavior:: EmulatorTrace ()
trace_close_nooracle_behavior = do
    ------- INITIALIZATION
    better1_wallet <- activateContractWallet (knownWallet 1) endpoints
    acceptor1_wallet <- activateContractWallet (knownWallet 2) endpoints
    oracle_wallet <- activateContractWallet (knownWallet 5) endpoints
    s_conf <- getSlotConfig
    
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

    void $ Emulator.waitUntilTime 55_000

    Last acceptor_scriptTxId <- observableState acceptor1_wallet
    case acceptor_scriptTxId of 
        Nothing -> Extras.logError @String "No Script TxOut TxId"
        Just x -> do
            acceptor1_wallet <- activateContractWallet (knownWallet 2) endpoints
            Extras.logInfo @String "Calling  End"
            callEndpoint @"cancel" acceptor1_wallet (x, CloseParams{
                        close_matchID     = "48656c6c6f204c6f727973"
                    })   

    s <- Emulator.waitNSlots 1

    Extras.logInfo $ "End of Simulation at slot " ++ show s
