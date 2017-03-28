{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Registry.Utils where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.SymmetricProcess

import Control.Monad (foldM, forM)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data MasterMsg   = MasterMsg   ProcessId deriving (Generic, Typeable)
data RegistryMsg = RegistryMsg ProcessId deriving (Generic, Typeable)
data ClientMsg   = ClientMsg   ProcessId deriving (Generic, Typeable)

instance Binary MasterMsg
instance Binary RegistryMsg
instance Binary ClientMsg
