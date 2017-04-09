{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AsyncPWithRace.PingServer (pingServer) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

pingServer :: ProcessId -> Process ()
pingServer master = do
  self <- getSelfPid
  send master self
  p <- expect :: Process ProcessId
  return ()
