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

------------------ REEDEMER ----------------------------------------------------------------------------------------

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
  