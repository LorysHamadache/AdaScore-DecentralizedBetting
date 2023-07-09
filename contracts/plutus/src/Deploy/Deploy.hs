{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy.Deploy where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger
import           Ledger.Address
import           Types
import           UtilsOnChain
import           BettingContract as BC

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

writeScript ::  String -> IO (Either (FileError ()) ())
writeScript file = writeValidator file validator

testdatum::BetDatum
testdatum = BetDatum {
    d_matchID     = "0",
    d_closedAt    = 1676342437000,
    d_resultlimAt = 1676352437000,
    d_result      = Unknown,
    d_creatorbet  = Win,
    d_odds        = 150,
    d_amount      = 10000000 ,
    d_fee         = getFeeCalculation $ 10000000,
    d_creator     = PaymentPubKeyHash "1ffabb5ea6eb9e9db3b9b5922e3f1ac735d60b0d8f17a84448fec9ad",
    d_acceptor    = PaymentPubKeyHash "1ffabb5ea6eb9e9db3b9b5922e3f1ac735d60b0d8f17a84448fec9ad",
    d_status      = AwaitingBet
    }

testreedemer::BetRedeemer
testreedemer = BetRedeemer
    {
        r_matchID  = "mymatchid",
        r_creator  = PaymentPubKeyHash "1ffabb5ea6eb9e9db3b9b5922e3f1ac735d60b0d8f17a84448fec9ad",
        r_action   = "cancel"
    } 