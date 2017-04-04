{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingSym2.PingServer (pingServer) where

import GHC.Base.Brisk
import PingSym2.Utils
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

pingServer :: ProcessId -> Process ()
pingServer m = do
  self <- getSelfPid
  I m2 <- expect
  send m $ M1 self
  expect :: Process ProcessId
  send m2 $ M2 self
  return ()
