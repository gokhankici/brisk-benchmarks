{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module DistDB.Worker (worker) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import DistDB.Utils  

worker :: () -> Process ()
worker _ = loop
  where
    loop = do
      request <- expect :: Process Request
      case request of
        Allocate pid key val -> return ()
        Lookup   pid key     -> send pid (Value "bar")
      loop
