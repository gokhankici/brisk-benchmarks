{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ConcDB.Utils where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.SymmetricProcess

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Request = Allocate ProcessId String
             | Lookup   ProcessId String
             deriving (Generic, Typeable)

data AllocateResponse = Allocated | Free                deriving (Generic, Typeable)
data LookupResponse   = Value String                    deriving (Generic, Typeable)
data SetRequest       = SetValue String                 deriving (Generic, Typeable)

instance Binary Request
instance Binary AllocateResponse
instance Binary LookupResponse
instance Binary SetRequest
