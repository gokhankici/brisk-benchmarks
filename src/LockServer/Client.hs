{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module LockServer.Client (client) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import LockServer.Utils

client :: ProcessId -> Process ()
client server = do
  me <- getSelfPid
  send server (Lock me)
  Ack <- expect
  selfSign Unlock >>= send server
  -- send server Unlock
  return ()
