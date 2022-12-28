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


module Types where

import qualified PlutusTx
import           Ledger                 hiding (singleton)
import qualified PlutusTx.Builtins      as Builtins
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Playground.Contract    (ToSchema)
import qualified Prelude                as Haskell
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)     


------------------ MatchBet --------------------------------------------------------------------------------------

data MatchBet = Win | Draw | Loss | Unknown
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

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
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq BetStatus where
    {-# INLINABLE (==) #-}
    AwaitingBet     == AwaitingBet     = True
    AwaitingResult  == AwaitingResult  = True
    _               == _               = False
PlutusTx.makeIsDataIndexed ''BetStatus [('AwaitingBet,0), ('AwaitingResult,1)]


------------------ DATUM ----------------------------------------------------------------------------------------

--deriving instance forall a (Eq a) => Eq (BetDatum a)

data BetDatum =
    BetDatum
    {
        d_matchID     :: Builtins.BuiltinByteString,
        d_closedAt    :: POSIXTime,
        d_resultlimAt    :: POSIXTime,
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



-- Didnt manage with StandaloneDeriving & ExistentialQuantification as the result is not inline and can't seem to find how to do it
-- (Eq MatchBet)
-- deriving instance Eq BetDatum

------------------ Redeemer ----------------------------------------------------------------------------------------

data BetRedeemer = 
    BetRedeemerAccept
    {
        r_matchID     :: Builtins.BuiltinByteString,
        r_creator     :: PaymentPubKeyHash
    } | BetRedeemerOracle
    {
        r_matchID     :: Builtins.BuiltinByteString,
        r_result      :: MatchBet
    }
PlutusTx.makeIsDataIndexed ''BetRedeemer [('BetRedeemerAccept,0),('BetRedeemerOracle,1)]
  