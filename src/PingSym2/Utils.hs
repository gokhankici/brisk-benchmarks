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

