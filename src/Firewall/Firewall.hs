{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Firewall.Firewall (firewall) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import Firewall.Utils

firewall :: ProcessId -> Process ()
firewall server = loop
  where
    loop = do
      msg <- expect
      case msg of
        GoodRequest _ -> send server msg
        BadRequest _  -> return ()
      loop
