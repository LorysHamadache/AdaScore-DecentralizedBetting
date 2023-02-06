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
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ImportQualifiedPost #-}


module Types where

import           PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Plutus.V2.Ledger.Api
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Prelude                as Haskell
import           GHC.Generics           (Generic)
import           Ledger.Address


------------------ MatchBet --------------------------------------------------------------------------------------

data MatchBet = Win | Draw | Loss | Unknown
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq MatchBet where
    {-# INLINABLE (==) #-}
    Win     == Win      = True
    Draw    == Draw     = True
    Loss    == Loss     = True
    Unknown == Unknown  = True
    _       == _        = False

PlutusTx.makeIsDataIndexed ''MatchBet [('Win,0), ('Draw,1), ('Loss,2), ('Unknown,3)]

------------------ BetStatus --------------------------------------------------------------------------------------

data BetStatus = AwaitingBet | AwaitingResult 
    deriving stock (Haskell.Show, Generic, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

instance Eq BetStatus where
    {-# INLINABLE (==) #-}
    AwaitingBet     == AwaitingBet     = True
    AwaitingResult  == AwaitingResult  = True
    _               == _               = False
PlutusTx.makeIsDataIndexed ''BetStatus [('AwaitingBet,0), ('AwaitingResult,1)]


------------------ DATUM ----------------------------------------------------------------------------------------
 
data BetDatum =
    BetDatum {
        d_matchID     :: BuiltinByteString,
        d_closedAt    :: POSIXTime,
        d_resultlimAt :: POSIXTime,
        d_result      :: MatchBet,
        d_creatorbet  :: MatchBet,
        d_odds        :: Integer,
        d_amount      :: Integer,
        d_fee         :: Integer,
        d_creator     :: PaymentPubKeyHash,
        d_acceptor    :: PaymentPubKeyHash,
        d_status      :: BetStatus
    } |
    OracleDatum {
         o_matchID     :: BuiltinByteString,
         o_result      :: MatchBet,
         o_end         :: POSIXTime
    }
PlutusTx.makeIsDataIndexed ''BetDatum [('BetDatum,0), ('OracleDatum,1)]

instance Eq BetDatum where
    {-# INLINABLE (==) #-}
    a == b     =  (d_matchID a == d_matchID b) 
               && (d_closedAt a == d_closedAt b) 
               && (d_resultlimAt a == d_resultlimAt b) 
               && (d_result a == d_result b) 
               && (d_creatorbet a == d_creatorbet b)
               && (d_odds a == d_odds b)
               && (d_amount a == d_amount b)
               && (d_fee a == d_fee b)
               && (d_creator a == d_creator b)
               && (d_acceptor a == d_acceptor b)
               && (d_status a == d_status b)


data BetRedeemer = 
    BetRedeemer
    {
        r_matchID     :: BuiltinByteString,
        r_creator     :: PaymentPubKeyHash,
        r_action      :: BuiltinByteString
    } 
PlutusTx.makeIsDataIndexed ''BetRedeemer [('BetRedeemer,0)]
  