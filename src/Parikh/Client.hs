{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Parikh.Client (client) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import Parikh.Utils

client :: ProcessId -> Process ()
client server = do
  self <- getSelfPid
  send server (ParikhInit self 0)
  ParikhOK <- expect
  send server (ParikhSet 1)
  send server ParikhBye
  return ()
