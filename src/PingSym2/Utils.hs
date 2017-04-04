{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingSym2.Utils where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.SymmetricProcess
import Control.Distributed.Process.Brisk

import Control.Monad (foldM, forM)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

workerSize :: Int
workerSize =  10

getNodes :: Int -> Process [NodeId]
getNodes n = do node <- getSelfNode
                return $ replicate n node

data Master1Msg = M1 ProcessId
                  deriving (Generic, Typeable)
data Master2Msg = M2 ProcessId
                  deriving (Generic, Typeable)
data Init = I ProcessId
                  deriving (Generic, Typeable)
instance Binary Master1Msg
instance Binary Master2Msg
instance Binary Init
