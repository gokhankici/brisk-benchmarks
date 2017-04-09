{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module FirewallWrongPid.Server (server) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import FirewallWrongPid.Utils

server :: () -> Process ()
server _ = loop 
  where
    loop = do msg <- expect
              case msg of
                Fwd (GoodRequest p) -> send p SrvResponse
                Fwd (BadRequest _)  -> fail "BadRequest"
              loop
