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

module TxConstruction where

import           Types
import           Utils
import           BettingContract
import           UtilsOnChain

import           Control.Monad          hiding (fmap)
import           Data.Map               as Map
import           Data.Text              (Text,pack)
import           Data.Monoid            (Last (..))
import           Text.Printf            (printf)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Tuple.Extra       (uncurry3)

import           PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Plutus.V2.Ledger.Api
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import qualified Prelude                as Haskell
import           Prelude                (Semigroup (..))
import           GHC.Generics           (Generic)


import           Ledger                 (minAdaTxOut,interval)
import           Ledger.Tx             
import           Ledger.Address
import qualified Ledger.Ada  as Ada
import           Ledger.Constraints     as Constraints
import           Ledger.Bytes
import           Plutus.V2.Ledger.Tx
import           Plutus.Contract




--import           Ledger                 hiding (singleton)
--import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
--import           Playground.Types       (KnownCurrency (..))
--import           Prelude                (Semigroup (..), String,uncurry,show)
--
--import qualified PlutusTx.Builtins      as Builtins




-- OFF CHAIN TYPES  -- 

data CreateParams = 
    CreateParams {
        create_matchID     :: BuiltinByteString,
        create_closedAt    :: POSIXTime,
        create_resultAt    :: POSIXTime,
        create_creatorbet  :: MatchBet,
        create_odds        :: Integer,
        create_amount      :: Integer
        } 
        deriving stock (Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

data AcceptParams =
    AcceptParams {
        accept_matchID     :: BuiltinByteString,
        accept_creator     :: PaymentPubKeyHash
        }
        deriving stock (Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)
        
data OracleParams =
    OracleParams {
        oracle_matchID     :: BuiltinByteString,
        oracle_result      :: MatchBet,
        oracle_end         :: POSIXTime
        }
        deriving stock (Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

data CloseParams =
    CloseParams {
        close_matchID     :: BuiltinByteString
    }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)


acceptParamsToRedeemer :: AcceptParams -> BetRedeemer
acceptParamsToRedeemer p = BetRedeemer {r_creator = accept_creator p , r_matchID = accept_matchID p, r_action = "accept"}

closeParamsToRedeemer :: CloseParams -> BetRedeemer
closeParamsToRedeemer p = BetRedeemer {r_creator = PaymentPubKeyHash "" ,r_matchID = close_matchID p, r_action = "close"} 

cancelParamsToRedeemer :: CloseParams -> BetRedeemer
cancelParamsToRedeemer p = BetRedeemer {r_creator = PaymentPubKeyHash "" ,r_matchID = close_matchID p, r_action = "cancel"} 

-- OFF CHAIN TX CONSTRUCTORS
create :: AsContractError e => CreateParams -> Contract (Last TxId) s e ()
create param = do
    Plutus.Contract.logInfo @Haskell.String $ "Create Start" 
    pkh <- Plutus.Contract.ownFirstPaymentPubKeyHash
    let datum = BetDatum {
        d_matchID     = create_matchID param,
        d_closedAt    = create_closedAt param,
        d_resultlimAt = create_resultAt param,
        d_result      = Unknown,
        d_creatorbet  = create_creatorbet param ,
        d_odds        = create_odds param,
        d_amount      = create_amount param ,
        d_fee         = getFeeCalculation $ create_amount param,
        d_creator     = pkh,
        d_acceptor    = pkh,
        d_status      = AwaitingBet
      }
    let tx = mustPayToTheScriptWithInlineDatum datum (Ada.lovelaceValueOf $ (d_amount datum + d_fee datum))
    ledgerTx <- submitTxConstraints tValidator tx
    let txid = getCardanoTxId ledgerTx
    void $ awaitTxConfirmed $ txid                            
    tell $ Last $ Just $ txid
    Plutus.Contract.logInfo @Haskell.String $ "Create End" 


accept :: AsContractError Text => TxId -> AcceptParams -> Contract (Last TxId) s Text ()
accept txin param = do
    Plutus.Contract.logInfo @Haskell.String $ "Accept Start" 
    pkh <- Plutus.Contract.ownFirstPaymentPubKeyHash
    utxos <- utxosAt scrAddress
    let redeemer = acceptParamsToRedeemer param
    let accepted_utxos =  Map.filterWithKey  (\x _ -> txOutRefId x == txin) utxos
    input <- case Map.toList accepted_utxos of
                [(oref, o)] -> return (oref,o)
                _ -> Plutus.Contract.throwError $ pack $ printf "Invalid UTXO (too much or no relevant bet)"
    input_datum <-  getTxDatum $ snd input
    let output_datum = input_datum{d_acceptor = pkh, d_status = AwaitingResult}
    let tx = mustSpendScriptOutput (fst input) (Redeemer $ PlutusTx.toBuiltinData redeemer) <>
             mustPayToTheScriptWithInlineDatum (output_datum) (Ada.lovelaceValueOf $ (PlutusTx.Prelude.divide ((d_amount output_datum) * (d_odds output_datum)) 100)+ (d_fee output_datum)) <>
             mustValidateIn (to $ (d_closedAt input_datum)-2)
    let lookups = Constraints.unspentOutputs accepted_utxos   <> 
                  Constraints.typedValidatorLookups tValidator                                                                  
    Plutus.Contract.logInfo @Haskell.String $ "Accept End" ++ Haskell.show (to $ (d_closedAt input_datum)- 1)
    ledgerTx <- submitTxConstraintsWith @BetType lookups tx                 
    let txid = getCardanoTxId ledgerTx
    Plutus.Contract.logInfo @Haskell.String $ "Accept End" ++ Haskell.show (d_closedAt input_datum)
    void $ awaitTxConfirmed $ txid
    tell $ Last $ Just $ txid
                            


oracle :: AsContractError Text => OracleParams -> Contract (Last TxId) s Text ()
oracle param = do
    Plutus.Contract.logInfo @Haskell.String $ "Oracle Start" 
    let datum = OracleDatum {
        o_matchID = oracle_matchID param,
        o_result = oracle_result param,
        o_end = oracle_end param
    }
    let txcons_oracle  = mustPayToAddressWithInlineDatum oracle_address (Datum $ PlutusTx.toBuiltinData datum) (Ada.toValue minAdaTxOut)
    let tx_signature = mustBeSignedBy oracle_ppkh
    ledgerTx <- submitTxConstraints tValidator (txcons_oracle <> tx_signature)                                                                   
    let txid = getCardanoTxId ledgerTx
    Plutus.Contract.logInfo @Haskell.String $ "Oracle End1"
    void $ awaitTxConfirmed $ txid                    
    tell $ Last $ Just $ txid
    Plutus.Contract.logInfo @Haskell.String $ "Oracle End"  

cancel :: AsContractError Text => TxId -> CloseParams -> Contract (Last TxId) s Text ()
cancel txin param = do
    Plutus.Contract.logInfo @Haskell.String $ "Cancel Start" 
    utxos <- utxosAt scrAddress
    pkh <- Plutus.Contract.ownFirstPaymentPubKeyHash
    let redeemer = cancelParamsToRedeemer param
    let accepted_utxos_map =  Map.filterWithKey  (\x _ -> txOutRefId x == txin) utxos  
    let accepted_utxos = Map.toList $ accepted_utxos_map
    input <- case accepted_utxos of
                        [(oref, o)] -> return (oref,o) 
                        _ -> Plutus.Contract.throwError $ pack $ printf "No UTXO found"
    input_datum <- getTxDatum $ snd input
    let txcons_script  = mustSpendScriptOutput (fst input) (Redeemer $ PlutusTx.toBuiltinData redeemer)
    let txcons_creator  = mustPayToPubKey (d_creator input_datum) (Ada.lovelaceValueOf (d_amount input_datum + d_fee input_datum))
    let txcons_bet = mustPayToPubKey (d_acceptor input_datum) (Ada.lovelaceValueOf (PlutusTx.Prelude.divide ((d_amount input_datum) * (d_odds input_datum - 100)) 100))
    let tx_validatepostrlim = mustValidateIn  $ from (d_resultlimAt input_datum + 1)
    let tx_signature = mustBeSignedBy pkh
    tx_constraints <-
        case d_status input_datum of
            AwaitingBet -> return $ txcons_script <> txcons_creator <> tx_signature
            AwaitingResult -> return $ txcons_script <> txcons_creator <> txcons_bet <> tx_validatepostrlim <> tx_signature
    let lookups = Constraints.unspentOutputs accepted_utxos_map   <> 
                  Constraints.typedValidatorLookups tValidator                                                 
    ledgerTx <- submitTxConstraintsWith @BetType lookups tx_constraints 
    let txid = getCardanoTxId ledgerTx
    void $ awaitTxConfirmed $ txid                            
    tell $ Last $ Just $ txid
    Plutus.Contract.logInfo @Haskell.String $ "Cancel End " 

close :: AsContractError Text => TxId -> TxId -> CloseParams -> Contract (Last TxId) s Text ()
close txin oracletx param = do
    Plutus.Contract.logInfo @Haskell.String $ "Close Start"
    let redeemer = closeParamsToRedeemer param
    -- GET SCRIPT INPUT
    utxos <- utxosAt scrAddress
    pkh <- Plutus.Contract.ownFirstPaymentPubKeyHash
    let accepted_utxos_map =  Map.filterWithKey  (\x _ -> txOutRefId x == txin) utxos
    let accepted_utxos = Map.toList $ accepted_utxos_map                              
    input <- case accepted_utxos of
                        [(oref, o)] -> return (oref,o) 
                        _ -> Plutus.Contract.throwError $ pack $ printf "No UTXO found"
    input_datum <- getTxDatum $ snd input
    let txcons_script  = mustSpendScriptOutput (fst input) (Redeemer $ PlutusTx.toBuiltinData redeemer)
    let tx_validatein = mustValidateIn $ interval (d_closedAt input_datum + 1) (d_resultlimAt input_datum - 1) 
    -- GET ORACLE REFERENCE INPUT
    utxos_addr <- utxosAt oracle_address
    let oracleref_utxos = Map.filterWithKey  (\x _ -> txOutRefId x == oracletx) utxos_addr
    let acceptedoracle_utxos = [head $ Map.toList $ oracleref_utxos]                         
    input_reforacle <- case acceptedoracle_utxos of
                        [(oref, o)] -> return (oref,o) 
                        _ -> Plutus.Contract.throwError $ pack $ printf "No UTXO found"
    oracle_datum <- getTxDatumOracle $ snd input_reforacle
    let tx_refinpur = mustReferenceOutput $ fst input_reforacle
    -- CREATE OUTPUT TO WINNER
    tx_outputwinner <- getResultTx (o_result oracle_datum) (snd input)
    let txcons_output  = mustPayToPubKey (fst tx_outputwinner) (Ada.lovelaceValueOf (snd tx_outputwinner))
    -- CREATE OUTPUT TO SERVICE FEE
    let tx_outputfees = d_fee $ input_datum
    let tx_signature = mustBeSignedBy pkh
    let txfee_output = mustPayToPubKey oracle_ppkh (Ada.lovelaceValueOf $ tx_outputfees)
    let lookups = Constraints.unspentOutputs accepted_utxos_map   <> 
                  Constraints.typedValidatorLookups tValidator <>
                  Constraints.unspentOutputs oracleref_utxos                                                                            
    ledgerTx <- submitTxConstraintsWith @BetType lookups (txcons_script <> txcons_output <> txfee_output <> tx_validatein <> tx_refinpur <> tx_signature)                                
    let txid = getCardanoTxId ledgerTx 
    void $ awaitTxConfirmed $ txid                           
    tell $ Last $ Just $ txid
    Plutus.Contract.logInfo @Haskell.String $ "Close End"                                         
    


----- SCHEMAS & ENDPOINTS

type BettingSchema =
            Endpoint "create" CreateParams  
        .\/ Endpoint "accept" (TxId, AcceptParams)
        .\/ Endpoint "cancel" (TxId, CloseParams)
        .\/ Endpoint "close"  (TxId,TxId,CloseParams)
        .\/ Endpoint "oracle" OracleParams


create_endpoint :: Promise (Last TxId) BettingSchema Text ()
create_endpoint = endpoint @"create" create

accept_endpoint :: Promise (Last TxId) BettingSchema Text ()
accept_endpoint = endpoint @"accept" $ Haskell.uncurry accept

oracle_endpoint :: Promise (Last TxId) BettingSchema Text ()
oracle_endpoint = endpoint @"oracle" $ oracle

close_endpoint :: Promise (Last TxId) BettingSchema Text ()
close_endpoint = endpoint @"close" $  uncurry3  close

cancel_endpoint :: Promise (Last TxId) BettingSchema Text ()
cancel_endpoint = endpoint @"cancel" $ Haskell.uncurry cancel

endpoints :: Contract (Last TxId) BettingSchema Text ()
endpoints = Plutus.Contract.selectList [create_endpoint,accept_endpoint, oracle_endpoint, close_endpoint, cancel_endpoint]