{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Firewall.Utils where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.SymmetricProcess

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Request = GoodRequest ProcessId
             | BadRequest  ProcessId
             deriving (Generic, Typeable)

data Response = Response deriving (Generic, Typeable)

instance Binary Request
instance Binary Response
