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


module Tests where

import           Types
import           Utils
import           TxConstruction
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
import           Prelude                (IO, show)
import           Control.Monad.Freer.Extras           as Extras
import           Control.Monad          hiding (fmap)
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger.TimeSlot
import           Data.Default           (Default (..))
import           Plutus.Contract.Test
import qualified Plutus.V1.Ledger.Ada   as Ada
import           Control.Lens
import           Test.Tasty


main = defaultMain 
    (testGroup "Normal Behavior" [test_normal_creator, test_normal_acceptor])

----------------- CONFIG ---------------------------------

traceconf :: TraceConfig
traceconf = def

emulconf :: EmulatorConfig
emulconf = def{_slotConfig = SlotConfig 1000 0}

checkopt :: CheckOptions
checkopt = defaultCheckOptions & emulatorConfig .~ emulconf


---------------- NORMAL BEHAVIOR TESTS --------------------
trace_normal_acceptor::EmulatorTrace ()
trace_normal_acceptor = do
    ------- INITIALIZATION
    better1_wallet <- activateContractWallet (knownWallet 1) endpoints
    acceptor1_wallet <- activateContractWallet (knownWallet 2) endpoints
    oracle_wallet <- activateContractWallet (knownWallet 5) endpoints
    s_conf <- getSlotConfig
    
    ------- EXECUTION
    callEndpoint @"create" better1_wallet $ CreateParams {
        create_matchID     = "48656c6c6f204c6f727973",
        create_closedAt    = 10000,
        create_resultAt    = 50000,
        create_creatorbet  = Win,
        create_odds        = 150,
        create_amount      = 50000000
        }
    t <- Emulator.waitUntilTime 3000
    callEndpoint @"accept" acceptor1_wallet $ AcceptParams {
        accept_matchID     = "48656c6c6f204c6f727973",
        accept_creator     = mockWalletPaymentPubKeyHash (knownWallet 1)
        } 
    void $ Emulator.waitNSlots 3
    callEndpoint @"oracle" oracle_wallet $ OracleParams {
        oracle_matchID     = "48656c6c6f204c6f727973",
        oracle_result     = Loss
        }  
    s <- Emulator.waitNSlots 1

    Extras.logInfo $ "End of Simulation at slot " ++ show s

test_normal_acceptorIO :: IO ()
test_normal_acceptorIO = runEmulatorTraceIO' traceconf emulconf trace_normal_acceptor 

test_normal_acceptor :: TestTree
test_normal_acceptor = checkPredicateOptions
            (checkopt)
            "Normal Behavior - Acceptor Winning"
            (
                walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-bet_creator) + Ada.lovelaceValueOf (-fees)) .&&.
                walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf (bet_creator)) .&&.
                walletFundsChange (knownWallet 5) (Ada.lovelaceValueOf (fees))
            )
            trace_normal_acceptor
            where
                bet_creator = 50_000_000
                odds = 150
                fees = getFeeCalculation bet_creator

            
trace_normal_creator::EmulatorTrace ()
trace_normal_creator = do
    ------- INITIALIZATION
    better1_wallet <- activateContractWallet (knownWallet 1) endpoints
    acceptor1_wallet <- activateContractWallet (knownWallet 2) endpoints
    oracle_wallet <- activateContractWallet (knownWallet 5) endpoints
    s_conf <- getSlotConfig
    
    ------- EXECUTION
    callEndpoint @"create" better1_wallet $ CreateParams {
        create_matchID     = "48656c6c6f204c6f727973",
        create_closedAt    = 10000,
        create_resultAt    = 50000,
        create_creatorbet  = Win,
        create_odds        = 150,
        create_amount      = 50000000
        }
    t <- Emulator.waitUntilTime 3000
    callEndpoint @"accept" acceptor1_wallet $ AcceptParams {
        accept_matchID     = "48656c6c6f204c6f727973",
        accept_creator     = mockWalletPaymentPubKeyHash (knownWallet 1)
        } 
    void $ Emulator.waitNSlots 3
    callEndpoint @"oracle" oracle_wallet $ OracleParams {
        oracle_matchID     = "48656c6c6f204c6f727973",
        oracle_result     = Win
        }  
    s <- Emulator.waitNSlots 1

    Extras.logInfo $ "End of Simulation at slot " ++ show s

test_normal_creatorIO :: IO ()
test_normal_creatorIO = runEmulatorTraceIO' traceconf emulconf trace_normal_creator 

test_normal_creator :: TestTree
test_normal_creator = checkPredicateOptions
            (checkopt)
            "Normal Behavior - Creator Winning"
            (
                walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (bet_acceptor) + Ada.lovelaceValueOf (-fees)) .&&.
                walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf (-bet_acceptor)) .&&.
                walletFundsChange (knownWallet 5) (Ada.lovelaceValueOf (fees))
            )
            trace_normal_creator
            where
                bet_creator = 50_000_000
                bet_acceptor = PlutusTx.Prelude.divide (bet_creator*odds) 100 - bet_creator
                odds = 150
                fees = getFeeCalculation bet_creator









test2 :: IO ()
test2 = runEmulatorTraceIO $ do
    better1_wallet <- activateContractWallet (knownWallet 1) endpoints
    acceptor1_wallet <- activateContractWallet (knownWallet 2) endpoints
    better2_wallet <- activateContractWallet (knownWallet 3) endpoints
    acceptor2_wallet <- activateContractWallet (knownWallet 4) endpoints
    oracle_wallet <- activateContractWallet (knownWallet 5) endpoints
    
    
    callEndpoint @"create" better1_wallet $ CreateParams {
        create_matchID     = "48656c6c6f204c6f727973",
        create_closedAt    = 10,
        create_resultAt    = 20,
        create_creatorbet  = Win,
        create_odds        = 150,
        create_amount      = 50000000
        }
    callEndpoint @"create" better2_wallet $ CreateParams {
        create_matchID     = "48656c6c6f204c6f727973",
        create_closedAt    = 10,
        create_resultAt    = 20,
        create_creatorbet  = Draw,
        create_odds        = 200,
        create_amount      = 10000000
        } 
    void $ Emulator.waitNSlots 3
    callEndpoint @"accept" acceptor1_wallet $ AcceptParams {
        accept_matchID     = "48656c6c6f204c6f727973",
        accept_creator     = mockWalletPaymentPubKeyHash (knownWallet 1)
        } 
    void $ Emulator.waitNSlots 1
    callEndpoint @"accept" acceptor2_wallet $ AcceptParams {
        accept_matchID     = "48656c6c6f204c6f727973",
        accept_creator     = mockWalletPaymentPubKeyHash (knownWallet 3)
        }  

    void $ Emulator.waitNSlots 20
    callEndpoint @"oracle" oracle_wallet $ OracleParams {
        oracle_matchID     = "48656c6c6f204c6f727973",
        oracle_result     = Loss
        }  
    s <- Emulator.waitNSlots 5

    Extras.logInfo $ "End of Simulation at slot " ++ show s   