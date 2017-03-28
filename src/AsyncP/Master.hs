{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AsyncP.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import AsyncP.PingServer

remotable [ 'pingServer ]

master :: [NodeId] -> Process ()
master nodes = do
  self <- getSelfPid
  pingServers <- spawnSymmetric nodes $ $(mkBriskClosure 'pingServer) ()
  forM pingServers (\p -> send p self)
  forM pingServers (\_ -> do _ <- expect :: Process ProcessId
                             return ())
  return ()
