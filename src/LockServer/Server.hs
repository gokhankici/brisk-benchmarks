{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module LockServer.Server (server) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import LockServer.Utils

server :: (SymSet ProcessId) -> Process ()
server clients = do
  forM clients (\_ -> do Lock p <- expect
                         send p Ack
                         -- Unlock <- expect
                         expectFrom p :: Process Unlock
                         return ())
  return ()
