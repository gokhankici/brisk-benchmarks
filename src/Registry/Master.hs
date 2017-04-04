{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Registry.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import Control.Monad (forM, foldM)

-- import Registry.Client
-- import Registry.Registry
import Registry.Utils  

client :: () -> Process ()
client _ = do (RegistryMsg reg) <- expect
              self <- getSelfPid
              send reg (ClientMsg self)

registry :: [NodeId] -> Process ()
registry ns = do (MasterMsg master) <- expect
                 foldM (\_ _ -> do
                           (ClientMsg _c) <- expect
                           return ()) () ns
                 send master OkMsg
                 return ()

remotable [ 'client, 'registry ]

master :: (NodeId,[NodeId]) -> Process ()
master (node,nodes) = do
  clients <- spawnSymmetric nodes $ $(mkBriskClosure 'client) ()
  regPid  <- spawn node $ $(mkBriskClosure 'registry) nodes

  self <- getSelfPid
  send regPid (MasterMsg self)
  forM clients (\c -> send c (RegistryMsg regPid))

  OkMsg <- expect
  return ()
