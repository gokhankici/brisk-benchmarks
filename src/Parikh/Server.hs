{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Parikh.Server (server) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import Parikh.Utils

server :: () -> Process ()
server _ = do
  (ParikhInit pid x) <- expect
  send pid ParikhOK
  serve x
  where
    serve x = do msg <- expect
                 case msg of
                   ParikhInit _ _ -> fail "Cannot init twice"
                   ParikhSet y    -> serve y
                   ParikhGet pid  -> send pid x >> serve x
                   ParikhBye      -> return () -- serve x
