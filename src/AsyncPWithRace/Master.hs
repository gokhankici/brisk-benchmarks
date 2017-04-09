{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AsyncPWithRace.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import AsyncPWithRace.PingServer

remotable [ 'pingServer ]

master :: [NodeId] -> Process ()
master nodes = do
  self <- getSelfPid
  pingServers <- spawnSymmetric nodes $ $(mkBriskClosure 'pingServer) self
  forM pingServers (\p -> send p self)
  forM pingServers (\_ -> do _ <- expect :: Process ProcessId
                             return ())
  return ()
