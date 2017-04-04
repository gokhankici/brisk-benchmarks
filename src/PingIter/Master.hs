{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingIter.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.Process.Brisk
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM, foldM)

import PingIter.PingServer

remotable [ 'pingServer ]

getNodes :: Int -> Process [NodeId]
getNodes n = do node <- getSelfNode
                return $ replicate n node

workerSize :: Int
workerSize  = 10

master :: Process ()
master = do
  nodes <- getNodes workerSize
  self <- getSelfPid
  pingServers <- spawnSymmetric nodes $ $(mkBriskClosure 'pingServer) ()
  forM pingServers (\p -> send p self)
  foldM (\_ _ -> do expect :: Process ProcessId
                    return ()) () nodes
  return ()
