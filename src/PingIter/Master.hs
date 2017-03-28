{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingIter.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM, foldM)

import PingIter.PingServer

remotable [ 'pingServer ]

master :: [NodeId] -> Process ()
master nodes = do
  self <- getSelfPid
  pingServers <- spawnSymmetric nodes $ $(mkBriskClosure 'pingServer) ()
  let n = length pingServers
  forM pingServers (\p -> send p self)
  foldM (\_ _ -> do _p <- expect :: Process ProcessId
                    return ()) () [1..n]
  return ()
