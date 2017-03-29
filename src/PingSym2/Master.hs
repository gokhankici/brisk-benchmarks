{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingSym2.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM, foldM)

import PingSym2.PingServer
import PingSym2.Master2
import PingSym2.Utils

remotable [ 'pingServer, 'master2 ]

master :: NodeId -> [NodeId] -> Process ()
master node nodes = do
  self <- getSelfPid
  m2 <- spawn node $ $(mkBriskClosure 'master2) ()
  pingServers <- spawnSymmetric nodes $ $(mkBriskClosure 'pingServer) (self,m2)
  forM pingServers (\_ -> do p <- expect :: Process ProcessId
                             send p self)
  return ()

main :: Process ()
main = do
  node <- getSelfNode
  nodes <- getNodes workerSize
  master node nodes
  
