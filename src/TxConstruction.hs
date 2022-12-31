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
import           Prelude                (Semigroup (..), String,uncurry,show)
import           Text.Printf            (printf)
import           Prelude                (Show)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)  
import           Playground.Contract    (ToSchema)
import qualified PlutusTx.Builtins      as Builtins
import           Data.Monoid            (Last (..))




-- OFF CHAIN TYPES  -- 

data CreateParams = 
    CreateParams {
        create_matchID     :: Builtins.BuiltinByteString,
        create_closedAt    :: POSIXTime,
        create_resultAt    :: POSIXTime,
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

data CloseParams =
    CloseParams {
        close_matchID     :: Builtins.BuiltinByteString
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)


acceptParamsToRedeemer :: AcceptParams -> BetRedeemer
acceptParamsToRedeemer p = BetRedeemerAccept {r_creator = accept_creator p , r_matchID = accept_matchID p}

oracleParamsToRedeemer :: OracleParams -> BetRedeemer
oracleParamsToRedeemer p = BetRedeemerOracle {r_matchID = oracle_matchID p, r_result = oracle_result p} 

closeParamsToRedeemer :: CloseParams -> BetRedeemer
closeParamsToRedeemer p = BetRedeemerClose {r_matchID = close_matchID p} 


-- OFF CHAIN TX CONSTRUCTORS
create :: AsContractError e => CreateParams -> Contract (Last TxId) s e ()
create param = do
    pkh <- Plutus.Contract.ownPaymentPubKeyHash
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
    let tx = mustPayToTheScript datum (Ada.lovelaceValueOf $ (d_amount datum + d_fee datum))
    ledgerTx <- submitTxConstraints tValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                  
    Plutus.Contract.logInfo @String $ printf "Bet Initialized"
    let scriptoutput_id = getTxIdWriter ledgerTx
    Plutus.Contract.logInfo  $ scriptoutput_id
    tell $ Last $ scriptoutput_id



accept :: AsContractError Text => TxId -> AcceptParams -> Contract (Last TxId) s Text ()
accept txin param = do
    pkh <- Plutus.Contract.ownPaymentPubKeyHash
    utxos <- utxosAt scrAddress
    let redeemer = acceptParamsToRedeemer param
    let accepted_utxos1 =  Map.filter (redeemerMatchingUtxo redeemer) utxos       
    let accepted_utxos =  Map.filterWithKey  (\x _ -> txOutRefId x == txin) accepted_utxos1                                                   
    input <- case Map.toList accepted_utxos of
                [(oref, o)] -> return (oref,o)
                _ -> Plutus.Contract.throwError $ pack $ printf "Invalid UTXO (too much or no relevant bet)"
    input_datum <- getTxDatum $ snd input
    let output_datum = input_datum{d_acceptor = pkh, d_status = AwaitingResult}
    let tx = mustSpendScriptOutput (fst input) (Redeemer $ PlutusTx.toBuiltinData redeemer) <>
             mustPayToTheScript (output_datum) (Ada.lovelaceValueOf $ (PlutusTx.Prelude.divide ((d_amount output_datum) * (d_odds output_datum)) 100)+ (d_fee output_datum)) <>
             mustValidateIn (to $ (d_closedAt input_datum)- 1)
    let lookups = Constraints.unspentOutputs accepted_utxos   <> 
                  Constraints.typedValidatorLookups tValidator <> 
                  Constraints.otherScript validator                                                                        
    ledgerTx <- submitTxConstraintsWith @BetType lookups tx
    Plutus.Contract.logInfo @String $ "Bet Accepted 31"                      
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                               
    Plutus.Contract.logInfo @String $ "Bet Accepted 41"

    let scriptoutput_id = getTxIdWriter ledgerTx
    Plutus.Contract.logInfo $ scriptoutput_id
    tell $ Last $ scriptoutput_id                                       


oracle :: AsContractError Text => TxId -> OracleParams -> Contract (Last TxId) s Text ()
oracle txin param = do
    utxos <- utxosAt scrAddress
    let redeemer = oracleParamsToRedeemer param
    let accepted_utxos_map1 = Map.filter (redeemerMatchingUtxo redeemer) utxos  
    let accepted_utxos_map =  Map.filterWithKey  (\x _ -> txOutRefId x == txin) accepted_utxos_map1  
    let accepted_utxos = Map.toList $ accepted_utxos_map                                            
    input <- case accepted_utxos of
                        [(oref, o)] -> return (oref,o) 
                        _ -> Plutus.Contract.throwError $ pack $ printf "No UTXO found"
    let txcons_script  = mustSpendScriptOutput (fst input) (Redeemer $ PlutusTx.toBuiltinData redeemer)
    tx_outputwinner <- getResultTx (r_result redeemer) (snd input)
    let txcons_output  = mustPayToPubKey (fst tx_outputwinner) (Ada.lovelaceValueOf (snd tx_outputwinner))
    tx_outputfees <- (getFeeTx . snd) input
    let txfee_output = mustPayToPubKey oracle_ppkh (Ada.lovelaceValueOf $ snd tx_outputwinner)
    let tx_signature = mustBeSignedBy oracle_ppkh

    input_datum <- getTxDatum $ snd input
    let tx_validatein = mustValidateIn $ interval (d_closedAt input_datum + 1) (d_resultlimAt input_datum - 1) 

    let lookups = Constraints.unspentOutputs accepted_utxos_map   <> 
                  Constraints.typedValidatorLookups tValidator <> 
                  Constraints.otherScript validator                                                                               
    ledgerTx <- submitTxConstraintsWith @BetType lookups (txcons_script <> txcons_output <> txfee_output <> tx_signature <> tx_validatein)                         
    tx_id <- awaitTxConfirmed $ getCardanoTxId ledgerTx                                               
    Plutus.Contract.logInfo @String $ "Oracle Submitted" 
    let test_output = getTxValueAt oracle_ppkh (PlutusTx.Prelude.map (\(x,y) -> x) (getCardanoTxOutRefs ledgerTx))
    Plutus.Contract.logInfo $ "Oracle Input Value: " ++ show (test_output)   
    Plutus.Contract.logInfo $ "Oracle Fee: " ++ show (d_fee input_datum)   

close :: AsContractError Text => TxId -> CloseParams -> Contract (Last TxId) s Text ()
close txin param = do
    Plutus.Contract.logInfo @String $ "TEST"  
    utxos <- utxosAt scrAddress
    let redeemer = closeParamsToRedeemer param
    let accepted_utxos_map1 = Map.filter (redeemerMatchingUtxo redeemer) utxos  
    let accepted_utxos_map =  Map.filterWithKey  (\x _ -> txOutRefId x == txin) accepted_utxos_map1  
    let accepted_utxos = Map.toList $ accepted_utxos_map

    input <- case accepted_utxos of
                        [(oref, o)] -> return (oref,o) 
                        _ -> Plutus.Contract.throwError $ pack $ printf "No UTXO found"
    input_datum <- getTxDatum $ snd input
    Plutus.Contract.logInfo @String $ "TEST2"  
    let txcons_script  = mustSpendScriptOutput (fst input) (Redeemer $ PlutusTx.toBuiltinData redeemer)
    let txcons_creator  = mustPayToPubKey (d_creator input_datum) (Ada.lovelaceValueOf (d_amount input_datum + d_fee input_datum))
    let txcons_bet = mustPayToPubKey (d_acceptor input_datum) (Ada.lovelaceValueOf (PlutusTx.Prelude.divide ((d_amount input_datum) * (d_odds input_datum - 100)) 100))
    let tx_validatepostrlim = mustValidateIn  $ from (d_resultlimAt input_datum + 1) 
    Plutus.Contract.logInfo @String $ "TEST3"  
    tx_constraints <-
        case d_status input_datum of
            AwaitingBet -> return $ txcons_script <> txcons_creator
            AwaitingResult -> return $ txcons_script <> txcons_creator <> txcons_bet <> tx_validatepostrlim
 
    let lookups = Constraints.unspentOutputs accepted_utxos_map   <> 
                  Constraints.typedValidatorLookups tValidator <> 
                  Constraints.otherScript validator 
    Plutus.Contract.logInfo @String $ "TEST4"              
                                                                  
    ledgerTx <- submitTxConstraintsWith @BetType lookups tx_constraints 

    Plutus.Contract.logInfo $ "TEST5   :   " ++  show (from (d_closedAt input_datum + 1))

    tx_id <- awaitTxConfirmed $ getCardanoTxId ledgerTx                                               
    Plutus.Contract.logInfo @String $ "Close Initiated"    





----- SCHEMAS & ENDPOINTS

type BettingSchema =
            Endpoint "create" CreateParams  
        .\/ Endpoint "accept" (TxId, AcceptParams)
        .\/ Endpoint "close"  (TxId, CloseParams)
        .\/ Endpoint "oracle" (TxId, OracleParams)


create_endpoint :: Promise (Last TxId) BettingSchema Text ()
create_endpoint = endpoint @"create" create

accept_endpoint :: Promise (Last TxId) BettingSchema Text ()
accept_endpoint = endpoint @"accept" $ Prelude.uncurry accept

oracle_endpoint :: Promise (Last TxId) BettingSchema Text ()
oracle_endpoint = endpoint @"oracle" $ Prelude.uncurry oracle

close_endpoint :: Promise (Last TxId) BettingSchema Text ()
close_endpoint = endpoint @"close" $ Prelude.uncurry close

endpoints :: Contract (Last TxId) BettingSchema Text ()
endpoints = Plutus.Contract.selectList [create_endpoint,accept_endpoint, oracle_endpoint, close_endpoint]

mkSchemaDefinitions ''BettingSchema
$(mkKnownCurrencies [])