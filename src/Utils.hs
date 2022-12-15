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




-- ON CHAIN HELPER FUNCTIONS



------ TX INPUTS

-- Check the Datum & The Address of the input

{-# INLINABLE isScriptInputValid #-}
isScriptInputValid :: TxInfo -> BetDatum -> Bool 
isScriptInputValid info betdatum = 
    case (txOutDatumHash . txInInfoResolved) input of
        Nothing -> False
        Just dh -> case findDatum dh info of
            Nothing -> False
            Just (Datum d) -> case (PlutusTx.fromBuiltinData d)::(Maybe BetDatum) of
                Nothing -> traceError "Error: Cannot decode Datum"
                Just x -> (x == betdatum )
    where
        input = getScriptInput $ txInfoInputs info    


{-# INLINABLE isScriptInput #-}
isScriptInput :: TxInInfo -> Bool
isScriptInput i = 
    case (txOutDatumHash . txInInfoResolved) i of
        Nothing -> False
        Just _  -> True


{-# INLINABLE getScriptInput #-}
getScriptInput :: [TxInInfo] -> TxInInfo
getScriptInput i = 
    case [x | x <- i, isScriptInput x] of
        [y] -> y
        _ -> traceError "Error: Expected 1 script input"





-- OFF CHAIN HELPER FUNCTIONS  --

redeemerMatchingUtxo :: BetRedeemer -> ChainIndexTxOut -> Bool
redeemerMatchingUtxo r o = case _ciTxOutDatum o of
    Left _          -> False
    Right (Datum d) -> case PlutusTx.fromBuiltinData d of
        Nothing -> False
        Just d2 -> case r of
                    (BetRedeemerAccept _ _) -> (r_matchID r == (d_matchID d2)) &&
                                               (r_creator r == (d_creator d2)) &&
                                               ((d_status d2) == AwaitingBet)
                    (BetRedeemerOracle _ _) -> (r_matchID r == (d_matchID d2)) &&
                                               ((d_status d2) == AwaitingResult)


--- UNUSED
acceptSlotMatchingUtxo :: POSIXTime -> ChainIndexTxOut -> Bool
acceptSlotMatchingUtxo s o = case _ciTxOutDatum o of
    Left _          -> False
    Right (Datum d) -> case PlutusTx.fromBuiltinData d of
        Nothing -> False
        Just d2 -> s >= (d_closedAt d2)

--- UNUSED
resultSlotMatchingUtxo :: POSIXTime -> ChainIndexTxOut -> Bool
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