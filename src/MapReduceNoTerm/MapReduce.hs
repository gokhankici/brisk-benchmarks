{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module MapReduceNoTerm.MapReduceNoTerm (main) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.Brisk
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import MapReduceNoTerm.Utils
import MapReduceNoTerm.Master

remotable [ 'master ]

main :: [Int] -> Process ()
main work = do node  <- getSelfNode
               nodes <- getNodes mapperCount
               master work (node, nodes)
               return ()
