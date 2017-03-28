{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingDet.PingServer (pingServer) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

pingServer :: ProcessId -> Process ()
pingServer m = do self <- getSelfPid
                  send m self
                  _x1 <- expect :: Process ProcessId
                  _x2 <- expect :: Process ProcessId
                  return ()
