{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AsyncP.PingServer (pingServer) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

pingServer :: () -> Process ()
pingServer _ = do self <- getSelfPid
                  p <- expect :: Process ProcessId
                  send p self
