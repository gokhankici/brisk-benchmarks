{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module MapReduceNoReduce.MapReduceNoReduce (main) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.Brisk
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import MapReduceNoReduce.Utils
import MapReduceNoReduce.Master

remotable [ 'master ]

main :: [Int] -> Process ()
main work = do node  <- getSelfNode
               nodes <- getNodes mapperCount
               master work (node, nodes)
               return ()
