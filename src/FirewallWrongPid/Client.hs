{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module FirewallWrongPid.Client (client) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import FirewallWrongPid.Utils

client :: ProcessId -> Process ()
client server = do
  self <- getSelfPid
  send server (GoodRequest self)
  Response s <- expect
  return ()
