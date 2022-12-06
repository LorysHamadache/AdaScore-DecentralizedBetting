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


module Tests where

import           Types
import           TxConstruction
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
import           Prelude                (IO, show)
import           Control.Monad.Freer.Extras           as Extras
import           Control.Monad          hiding (fmap)
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
 




test1 :: IO ()
test1 = runEmulatorTraceIO $ do
    better1_wallet <- activateContractWallet (knownWallet 1) endpoints
    acceptor1_wallet <- activateContractWallet (knownWallet 2) endpoints
    oracle_wallet <- activateContractWallet (knownWallet 5) endpoints
    
    
    callEndpoint @"create" better1_wallet $ CreateParams {
        create_matchID     = "48656c6c6f204c6f727973",
        create_closedAt    = 10,
        create_resultAt    = 20,
        create_creatorbet  = Win,
        create_odds        = 150,
        create_amount      = 50000000
        }
    void $ Emulator.waitNSlots 3
    callEndpoint @"accept" acceptor1_wallet $ AcceptParams {
        accept_matchID     = "48656c6c6f204c6f727973",
        accept_creator     = mockWalletPaymentPubKeyHash (knownWallet 1)
        } 
    void $ Emulator.waitNSlots 20
    callEndpoint @"oracle" oracle_wallet $ OracleParams {
        oracle_matchID     = "48656c6c6f204c6f727973",
        oracle_result     = Loss
        }  
    s <- Emulator.waitNSlots 5

    Extras.logInfo $ "End of Simulation at slot " ++ show s   

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