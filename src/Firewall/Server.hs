{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Firewall.Server (server) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import Firewall.Utils

server :: () -> Process ()
server _ = loop 
  where
    loop = do msg <- expect
              case msg of
                GoodRequest p -> do send p Response
                                    loop
                BadRequest _ -> return ()
