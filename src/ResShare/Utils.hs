{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ResShare.Utils where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.SymmetricProcess
import Control.Distributed.Process.Brisk

import Control.Monad (foldM, forM)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Lock   = Lock   deriving (Generic, Typeable)
data Ack    = Ack    deriving (Generic, Typeable)
data Unlock = Unlock deriving (Generic, Typeable)

instance Binary Lock
instance Binary Ack
instance Binary Unlock
