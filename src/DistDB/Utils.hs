{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module DistDB.Utils where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.SymmetricProcess

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Request = Allocate ProcessId String String
             | Lookup   ProcessId String
             deriving (Generic, Typeable)

data LookupResponse   = Value String                    deriving (Generic, Typeable)

instance Binary Request
instance Binary LookupResponse
