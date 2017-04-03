{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Registry.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import Control.Monad (forM, foldM)

import Registry.Client
import Registry.Registry
import Registry.Utils  

remotable [ 'client, 'registry ]

master :: (NodeId,[NodeId]) -> Process ()
master (node,nodes) = do
  let n = length nodes
  regPid <- spawn node $ $(mkBriskClosure 'registry) n
  clients <- spawnSymmetric nodes $ $(mkBriskClosure 'client) ()

  self <- getSelfPid
  send regPid (MasterMsg self)
  forM clients (\c -> send c (RegistryMsg regPid))

  OkMsg <- expect
  return ()
