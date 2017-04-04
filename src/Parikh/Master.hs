{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Parikh.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import Parikh.Client
import Parikh.Server
import Parikh.Utils

remotable [ 'client, 'server ]

master :: NodeId -> Process ()
master node = do
  serverPid <- spawn node $ $(mkBriskClosure 'server) ()
  clientPid <- spawn node $ $(mkBriskClosure 'client) serverPid
  return ()
