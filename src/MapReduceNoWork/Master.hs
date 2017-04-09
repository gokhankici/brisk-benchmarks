{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module MapReduceNoWork.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (foldM)

import MapReduceNoWork.Utils
import MapReduceNoWork.Queue

remotable [ 'queue ]

master :: [Int] -> (NodeId, [NodeId]) -> Process ()
master work (node, nodes) =
  do self <- getSelfPid
     queuePid <- spawn node $ $(mkBriskClosure 'queue) (nodes, work, self)

     -- -- Send the work list
     -- send queuePid (WorkSet [1..workCount])

     -- for k times ...
     foldM go () work
  where
    go _ i = do (Result n) <- expect :: Process Result
                return ()

