{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module MapReduce.Utils where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.SymmetricProcess

import Control.Monad (foldM, forM)
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data WorkSet = WorkSet [Int]             deriving (Generic, Typeable)
data Request = Request ProcessId         deriving (Generic, Typeable)
data Work    = Work ProcessId Int | Term deriving (Generic, Typeable)
data Result  = Result Int                deriving (Generic, Typeable)

instance Binary WorkSet
instance Binary Request
instance Binary Work
instance Binary Result

workCount :: Int
workCount =  15

mapperCount :: Int
mapperCount =  10

getNodes :: Int -> Process [NodeId]
getNodes n = do node <- getSelfNode
                return $ replicate n node

