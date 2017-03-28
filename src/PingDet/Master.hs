{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingDet.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import PingDet.PingServer

remotable [ 'pingServer ]

master :: [NodeId] -> Process ()
master nodes = do
  self <- getSelfPid
  pingServers <- spawnSymmetric nodes $ $(mkBriskClosure 'pingServer) self
  forM pingServers (\p -> do send p self
                             send p self
                             expectFrom p :: Process ProcessId
                             return ())
  return ()
