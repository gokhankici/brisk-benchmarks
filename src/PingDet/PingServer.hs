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
                  selfMsg <- selfSign self
                  send m selfMsg
                  expect :: Process ProcessId
                  expect :: Process ProcessId
                  return ()
