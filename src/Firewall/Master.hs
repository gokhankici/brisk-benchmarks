{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Firewall.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import Firewall.Client
import Firewall.Server
import Firewall.Firewall

remotable [ 'client, 'firewall ]

master :: [NodeId] -> NodeId -> Process ()
master nodes node = do
  me <- getSelfPid
  firewallPid <- spawn node $ $(mkBriskClosure 'firewall) me
  spawnSymmetric nodes $ $(mkBriskClosure 'client) firewallPid
  server ()
