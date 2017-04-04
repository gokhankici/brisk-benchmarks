{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module TwoBuyers.Utils where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.SymmetricProcess
import Control.Distributed.Process.Brisk

import Control.Monad (foldM, forM)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Title = Title ProcessId ProcessId String
           deriving (Generic, Typeable)

data Quote = Quote ProcessId Int
           deriving (Generic, Typeable)

data BuyerQuote = BuyerQuote ProcessId Int
                deriving (Generic, Typeable)

data Offer = OK   ProcessId
           | Quit ProcessId
           deriving (Generic, Typeable)

data OfferDetails = OfferDetails ProcessId String
                  deriving (Generic, Typeable)

data ShippingDate = ShippingDate Int Int Int
                  deriving (Generic, Typeable)

instance Binary Title
instance Binary Quote
instance Binary BuyerQuote
instance Binary Offer
instance Binary OfferDetails
instance Binary ShippingDate
