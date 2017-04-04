{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Auction.Utils where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.SymmetricProcess
import Control.Distributed.Process.Brisk

import Control.Monad (foldM, forM)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Sellers = Sellers (SymSet ProcessId)
             deriving (Generic, Typeable)

data Buyers = Buyers (SymSet ProcessId)
            deriving (Generic, Typeable)

data SellerInfo = Match ProcessId
                | Quit  ProcessId
                deriving (Generic, Typeable)

data BuyerInfo = Notify ProcessId
               | Stop   ProcessId
               deriving (Generic, Typeable)

data Price = Price ProcessId Int
           deriving (Generic, Typeable)

data Order = Order ProcessId
           deriving (Generic, Typeable)

instance Binary SellerInfo
instance Binary BuyerInfo
instance Binary Price
instance Binary Order
instance Binary Sellers
instance Binary Buyers
