{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Parikh.Utils where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.SymmetricProcess
import Control.Distributed.Process.Brisk

import Control.Monad (foldM, forM)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data ParikhMessage = ParikhInit ProcessId Int
                   | ParikhSet Int
                   | ParikhGet ProcessId
                   | ParikhBye
                   deriving (Generic, Typeable)

data ParikhOK = ParikhOK
              deriving (Generic, Typeable)

instance Binary ParikhMessage
instance Binary ParikhOK
