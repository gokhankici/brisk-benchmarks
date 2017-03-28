{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingSym2.PingServer (pingServer) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

pingServer :: (ProcessId,ProcessId) -> Process ()
pingServer (m,m2) = do
  self <- getSelfPid
  send m self
  _p <- expect :: Process ProcessId
  send m2 self
  return ()
