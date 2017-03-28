{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingSym.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM, foldM)

import PingSym.PingServer

remotable [ 'pingServer ]

master :: [NodeId] -> Process ()
master nodes = do
  self <- getSelfPid
  pingServers <- spawnSymmetric nodes $ $(mkBriskClosure 'pingServer) self
  forM pingServers (\_ -> do p <- expect :: Process ProcessId
                             send p self)
  return ()
