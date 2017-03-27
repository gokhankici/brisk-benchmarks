{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module MapReduce.MapReduce (main) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import MapReduce.Utils
import MapReduce.Master

remotable [ 'master ]

main :: Process ()
main =  do node  <- getSelfNode
           nodes <- getNodes mapperCount
           spawnLocal $ master (node, nodes)
           return ()
