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
--{-# LANGUAGE RecordWildCards     #-}

module BettingContract where

import           Control.Monad          hiding (fmap)
import           Data.Map               as Map
import           Data.Text              (Text,pack)
import           Data.Void              (Void)
import           Plutus.Contract
import           PlutusTx               (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins      as Builtins
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Ada             as Ada
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO,Semigroup (..), String,Show,undefined, show)
import qualified Prelude                as Haskell
import           Text.Printf            (printf)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)  
import qualified Plutus.Trace           as Trace
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
import           Control.Monad.Freer.Extras           as Extras

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- DATA TYPES -- 

data MatchBet = Win | Draw | Loss | Unknown
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq MatchBet where
    {-# INLINABLE (==) #-}
    Win     == Win      = True
    Draw    == Draw     = True
    Loss    == Loss     = True
    Unknown == Unknown  = True
    _       == _        = False

PlutusTx.makeIsDataIndexed ''MatchBet [('Win,0), ('Draw,1), ('Loss,2), ('Unknown,3)]


data BetStatus = AwaitingBet | AwaitingResult 
    deriving stock (Show, Generic, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq BetStatus where
    {-# INLINABLE (==) #-}
    AwaitingBet     == AwaitingBet     = True
    AwaitingResult  == AwaitingResult  = True
    _               == _               = False
PlutusTx.makeIsDataIndexed ''BetStatus [('AwaitingBet,0), ('AwaitingResult,1)]

data BetDatum = 
    BetDatum
    {
        d_matchID     :: Builtins.BuiltinByteString,
        d_closedAt    :: Slot,
        d_resultAt    :: Slot,
        d_result      :: MatchBet,
        d_creatorbet  :: MatchBet,
        d_odds        :: Integer,
        d_amount      :: Integer,
        d_fee         :: Integer,
        d_creator     :: PaymentPubKeyHash,
        d_acceptor    :: PaymentPubKeyHash,
        d_status      :: BetStatus
    }
PlutusTx.makeIsDataIndexed ''BetDatum [('BetDatum,0)]

data BetReedemer = 
    BetReedemerAccept
    {
        r_matchID     :: Builtins.BuiltinByteString,
        r_creator     :: PaymentPubKeyHash
    } | BetReedemerOracle
    {
        r_matchID     :: Builtins.BuiltinByteString,
        r_result      :: MatchBet
    }
PlutusTx.makeIsDataIndexed ''BetReedemer [('BetReedemerAccept,0),('BetReedemerOracle,1)]

data BetType
instance Scripts.ValidatorTypes BetType where
    type instance DatumType BetType = BetDatum
    type instance RedeemerType BetType = BetReedemer
  
-- VALIDATOR -- 

{-# INLINABLE mkValidator #-}
mkValidator :: BetDatum -> BetReedemer -> ScriptContext -> Bool
mkValidator _ _ _ = True


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
        
-- OFF CHAIN HELPER FUNCTIONS  --

redeemerMatchingUtxo :: BetReedemer -> ChainIndexTxOut -> Bool
redeemerMatchingUtxo r o = case _ciTxOutDatum o of
    Left _          -> False
    Right (Datum d) -> case PlutusTx.fromBuiltinData d of
        Nothing -> False
        Just d2 -> case r of
                    (BetReedemerAccept _ _) -> (r_matchID r == (d_matchID d2)) &&
                                               (r_creator r == (d_creator d2)) &&
                                               ((d_status d2) == AwaitingBet)
                    (BetReedemerOracle _ _) -> (r_matchID r == (d_matchID d2)) &&
                                               ((d_status d2) == AwaitingResult)


acceptSlotMatchingUtxo :: Slot -> ChainIndexTxOut -> Bool
acceptSlotMatchingUtxo s o = case _ciTxOutDatum o of
    Left _          -> False
    Right (Datum d) -> case PlutusTx.fromBuiltinData d of
        Nothing -> False
        Just d2 -> s >= (d_closedAt d2)


resultSlotMatchingUtxo :: Slot -> ChainIndexTxOut -> Bool
resultSlotMatchingUtxo s o = case _ciTxOutDatum o of
    Left _          -> False
    Right (Datum d) -> case PlutusTx.fromBuiltinData d of
        Nothing -> False
        Just d2 -> s >= (d_resultAt d2)



getTxDatum :: ChainIndexTxOut -> Contract w s Text BetDatum
getTxDatum o = case _ciTxOutDatum o of
    Left _ -> Plutus.Contract.throwError $ pack $ printf "No Datum found"
    Right (Datum d) -> case PlutusTx.fromBuiltinData d of
        Nothing -> Plutus.Contract.throwError $ pack $ printf "Error Decoding Datum"
        Just d2 -> return d2

getResultTx ::  MatchBet -> ChainIndexTxOut -> Contract w s Text (PaymentPubKeyHash, Integer)
getResultTx mr o = do
    datum <- getTxDatum o
    let amount = PlutusTx.Prelude.divide ((d_amount datum) * (d_odds datum)) 100
    case mr of
        Unknown -> Plutus.Contract.throwError $ pack $ printf "No match result provided"
        _ -> if d_creatorbet datum == mr
            then return (d_creator datum, amount)
            else return (d_acceptor datum, amount)

getFeeTx :: ChainIndexTxOut -> Contract w s Text Integer
getFeeTx o = do
    datum <- getTxDatum o
    let fees = d_fee datum
    return fees



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