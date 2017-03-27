{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AsyncP.PingServer (pingServer) where

import Control.Distributed.Process

pingServer :: () -> Process ()
pingServer _ = loop
  where loop = do self <- getSelfPid
                  p <- expect :: Process ProcessId
                  send p self
