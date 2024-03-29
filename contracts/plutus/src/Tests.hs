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
import           UtilsOnChain

import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
import           Prelude                (IO, show, String)
import           Control.Monad.Freer.Extras           as Extras
import           Control.Monad          hiding (fmap)
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger.TimeSlot
import           Plutus.Contract.Test
import qualified Ledger.Ada   as Ada
import           Ledger.Params
import           Control.Lens
import           Test.Tasty
import           Data.Monoid                  (Last (..))
import           Data.Default           (Default (..))

import           Traces.NormalBehaviorTrace
import           Traces.CloseBehaviorTrace



main = defaultMain (testGroup "Expected Behaviors" [grouptest_normal, grouptest_close])

grouptest_normal = testGroup "Normal Behavior" [test_normal_creator, test_normal_acceptor]
grouptest_close = testGroup "Close Behavior"  [test_close_nobet_behavior, test_close_nooracle_behavior]
----------------- CONFIG ---------------------------------

traceconf :: TraceConfig
traceconf = def

slotconf :: SlotConfig
slotconf = SlotConfig 1000 0

paramsconf :: Params
paramsconf = def{pSlotConfig = slotconf}

emulconf :: EmulatorConfig
emulconf = def{_params  = paramsconf}

checkopt :: CheckOptions
checkopt = defaultCheckOptions & emulatorConfig .~ emulconf

x=Win
y=Loss

---------------- NORMAL BEHAVIOR TESTS --------------------

test_normal_acceptor :: TestTree
test_normal_acceptor = checkPredicateOptions
            (checkopt)
            "Normal Behavior - Acceptor Winning"
            (
                walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-bet_creator) + Ada.lovelaceValueOf (-fees)) .&&.
                walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf (bet_creator)) .&&.
                walletFundsChange (knownWallet 5) (Ada.lovelaceValueOf (fees))
            )
            (trace_normal_behavior Loss)
            where
                bet_creator = 50_000_000
                odds = 150
                fees = getFeeCalculation bet_creator

test_normal_creator :: TestTree
test_normal_creator = checkPredicateOptions
            (checkopt)
            "Normal Behavior - Creator Winning"
            (
                walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (bet_acceptor) + Ada.lovelaceValueOf (-fees)) .&&.
                walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf (-bet_acceptor)) .&&.
                walletFundsChange (knownWallet 5) (Ada.lovelaceValueOf (fees))
            )
            (trace_normal_behavior Win)
            where
                bet_creator = 50_000_000
                bet_acceptor = PlutusTx.Prelude.divide (bet_creator*odds) 100 - bet_creator
                odds = 150
                fees = getFeeCalculation bet_creator

test_normal_behaviorIO :: MatchBet -> IO ()
test_normal_behaviorIO bres = runEmulatorTraceIO' traceconf emulconf (trace_normal_behavior bres)

----------------- CLOSE BEHAVIOR TESTS -------------------------

test_close_nobet_behavior :: TestTree
test_close_nobet_behavior = checkPredicateOptions
            (checkopt)
            "Close Behavior - No Bet Closed"
            (
                walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-bet_creator-fees) + Ada.lovelaceValueOf (bet_creator+fees))
            )
            (trace_close_nobet_behavior)
            where
                bet_creator = 50_000_000
                fees = getFeeCalculation bet_creator

test_close_nooracle_behavior :: TestTree
test_close_nooracle_behavior = checkPredicateOptions
            (checkopt)
            "Close Behavior - No Oracle Closed"
            (
                walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-bet_creator-fees) + Ada.lovelaceValueOf (bet_creator+fees))
            )
            (trace_close_nooracle_behavior)
            where
                bet_creator = 50_000_000
                fees = getFeeCalculation bet_creator

test_close_nobet_behaviorIO:: IO ()
test_close_nobet_behaviorIO = runEmulatorTraceIO' traceconf emulconf trace_close_nobet_behavior



ptest :: IO ()
ptest = runEmulatorTraceIO' traceconf emulconf $ do
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
        oracle_result = x,
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
                better1_wallet <- activateContractWallet (knownWallet 1) endpoints
                callEndpoint @"close" better1_wallet (scpinput,oclref, CloseParams{
                 close_matchID     = "48656c6c6f204c6f727973"
                 }) 
    
    s <- Emulator.waitNSlots 1

    Extras.logInfo $ "End of Simulation at slot " ++ show s