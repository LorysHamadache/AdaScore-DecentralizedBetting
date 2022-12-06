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

module TxConstruction where

import           Types
import           BettingContract
import           Utils
import           Control.Monad          hiding (fmap)
import           Data.Map               as Map
import           Data.Text              (Text,pack)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import           Ledger.Ada             as Ada
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (Semigroup (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           Prelude                (Show)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)  
import           Playground.Contract    (ToSchema)
import qualified PlutusTx.Builtins      as Builtins




-- OFF CHAIN TYPES  -- 

data CreateParams = 
    CreateParams {
        create_matchID     :: Builtins.BuiltinByteString,
        create_closedAt    :: Slot,
        create_resultAt    :: Slot,
        create_creatorbet  :: MatchBet,
        create_odds        :: Integer,
        create_amount      :: Integer
        } 
        deriving stock (Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

data AcceptParams =
    AcceptParams {
        accept_matchID     :: Builtins.BuiltinByteString,
        accept_creator     :: PaymentPubKeyHash
        }
        deriving stock (Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)
        
data OracleParams =
    OracleParams {
        oracle_matchID     :: Builtins.BuiltinByteString,
        oracle_result      :: MatchBet
        }
        deriving stock (Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)


acceptParamsToRedeemer :: AcceptParams -> BetReedemer
acceptParamsToRedeemer p = BetReedemerAccept {r_creator = accept_creator p , r_matchID = accept_matchID p}

oracleParamsToRedeemer :: OracleParams -> BetReedemer
oracleParamsToRedeemer p = BetReedemerOracle {r_matchID = oracle_matchID p, r_result = oracle_result p} 


-- OFF CHAIN TX CONSTRUCTORS
create :: AsContractError e => CreateParams -> Contract w s e ()
create param = do
    pkh <- Plutus.Contract.ownPaymentPubKeyHash
    let datum = BetDatum {
        d_matchID     = create_matchID param,
        d_closedAt    = create_closedAt param,
        d_resultAt    = create_resultAt param,
        d_result      = Unknown,
        d_creatorbet  = create_creatorbet param ,
        d_odds        = create_odds param,
        d_amount      = create_amount param ,
        d_fee         = PlutusTx.Prelude.divide (create_amount param * 5) 100 + 2000000,
        d_creator     = pkh,
        d_acceptor    = pkh,
        d_status      = AwaitingBet
      }
    let tx = mustPayToTheScript datum (Ada.lovelaceValueOf $ (d_amount datum + d_fee datum))
    ledgerTx <- submitTxConstraints tValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                  
    Plutus.Contract.logInfo @String $ printf "Bet Initialized"

accept :: AsContractError Text => AcceptParams -> Contract w s Text ()
accept param = do
    pkh <- Plutus.Contract.ownPaymentPubKeyHash
    utxos <- utxosAt scrAddress
    let redeemer = acceptParamsToRedeemer param
    let accepted_utxos =  Map.filter (redeemerMatchingUtxo redeemer) utxos                                                        
    input <- case Map.toList accepted_utxos of
                [(oref, o)] -> return (oref,o)
                _ -> Plutus.Contract.throwError $ pack $ printf "Invalid UTXO (too much or no relevant bet)"
    input_datum <- getTxDatum $ snd input
    let output_datum = input_datum{d_acceptor = pkh, d_status = AwaitingResult}
    let tx = mustSpendScriptOutput (fst input) (Redeemer $ PlutusTx.toBuiltinData redeemer) <>
             mustPayToTheScript (output_datum) (Ada.lovelaceValueOf $ (PlutusTx.Prelude.divide ((d_amount output_datum) * (d_odds output_datum)) 100)+1+ (d_fee output_datum))
    let lookups = Constraints.unspentOutputs accepted_utxos   <> 
                  Constraints.typedValidatorLookups tValidator <> 
                  Constraints.otherScript validator                                                                               
    ledgerTx <- submitTxConstraintsWith @BetType lookups tx                         
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                               
    Plutus.Contract.logInfo @String $ "Bet Accepted"                                       


oracle :: AsContractError Text => OracleParams -> Contract w s Text ()
oracle param = do
    utxos <- utxosAt scrAddress
    let redeemer = oracleParamsToRedeemer param
    let accepted_utxos_map = Map.filter (redeemerMatchingUtxo redeemer) utxos  
    let accepted_utxos = Map.toList $ accepted_utxos_map                                            
    input_utxos <- case accepted_utxos of
                        [] -> Plutus.Contract.throwError $ pack $ printf "No UTXO found"
                        _ -> return accepted_utxos
    let txcons_script  = mconcat [mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData redeemer) | oref <- fst <$> input_utxos]

    tx_outputvalues <- PlutusTx.Prelude.mapM ((getResultTx (r_result redeemer) ). snd) input_utxos
    let txcons_output  = mconcat [mustPayToPubKey  (fst otx) (Ada.lovelaceValueOf (snd otx)) | otx <- tx_outputvalues]

    tx_outputfees <- PlutusTx.Prelude.mapM (getFeeTx . snd) input_utxos
    let txfee_output = mconcat [mustPayToPubKey  (mockWalletPaymentPubKeyHash $ knownWallet 5) (Ada.lovelaceValueOf otx) | otx <- tx_outputfees]

    let lookups = Constraints.unspentOutputs accepted_utxos_map   <> 
                  Constraints.typedValidatorLookups tValidator <> 
                  Constraints.otherScript validator                                                                               
    ledgerTx <- submitTxConstraintsWith @BetType lookups (txcons_script <> txcons_output <> txfee_output)                         
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                               
    Plutus.Contract.logInfo @String $ "Bet Accepted"      


----- SCHEMAS & ENDPOINTS

type BettingSchema =
            Endpoint "create" CreateParams  
        .\/ Endpoint "accept" AcceptParams
        .\/ Endpoint "oracle" OracleParams

create_endpoint :: Promise () BettingSchema Text ()
create_endpoint = endpoint @"create" create

accept_endpoint :: Promise () BettingSchema Text ()
accept_endpoint = endpoint @"accept" accept

oracle_endpoint :: Promise () BettingSchema Text ()
oracle_endpoint = endpoint @"oracle" oracle

endpoints :: Contract () BettingSchema Text ()
endpoints = Plutus.Contract.selectList [create_endpoint, accept_endpoint, oracle_endpoint]

mkSchemaDefinitions ''BettingSchema
$(mkKnownCurrencies [])