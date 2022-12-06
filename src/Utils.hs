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

module Utils where


import           Types
import           Control.Monad          hiding (fmap)
import           Data.Text              (Text,pack)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Text.Printf            (printf)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)  
import           Playground.Contract    (ToSchema)
import qualified PlutusTx.Builtins      as Builtins

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