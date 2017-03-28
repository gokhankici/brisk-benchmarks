{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingSym.PingServer (pingServer) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

pingServer :: ProcessId -> Process ()
pingServer m = do self <- getSelfPid
                  send m self
                  _ <- expect :: Process ProcessId
                  return ()
