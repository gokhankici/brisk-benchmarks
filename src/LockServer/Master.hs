{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module LockServer.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import LockServer.Client
import LockServer.Server
import LockServer.Utils

remotable [ 'client ]

master :: [NodeId] -> Process ()
master nodes = do
  me <- getSelfPid
  clients <- spawnSymmetric nodes $ $(mkBriskClosure 'client) me
  server clients
