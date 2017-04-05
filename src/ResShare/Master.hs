{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ResShare.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import ResShare.Client
import ResShare.Server
import ResShare.Utils

remotable [ 'client, 'server ]

master :: [NodeId] -> Process ()
master nodes = do
  servers <- spawnSymmetric nodes $ $(mkBriskClosure 'server) ()
  clients <- spawnSymmetric nodes $ $(mkBriskClosure 'client) servers
  forM servers (\pid -> send pid clients)
  return ()
