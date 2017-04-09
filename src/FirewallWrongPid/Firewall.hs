{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module FirewallWrongPid.Firewall (firewall) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import FirewallWrongPid.Utils

firewall :: ProcessId -> Process ()
firewall server = loop
  where
    loop = do
      me  <- getSelfPid
      msg <- expect :: Process Request
      case msg of
        GoodRequest p -> do
          send server (Fwd (GoodRequest p))
          SrvResponse <- expect
          send p (Response SrvResponse)
        _ -> return ()
      loop
