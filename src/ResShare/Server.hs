{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ResShare.Server (server) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import ResShare.Utils

server :: () -> Process ()
server _ = do
  clients <- expect :: Process (SymSet ProcessId)
  loop clients
  where
    loop clients = do 
      forM clients (\c -> do Lock <- expectFrom c
                             (selfSign Ack) >>= send c
                             Unlock <- expectFrom c
                             return ())
      loop clients
