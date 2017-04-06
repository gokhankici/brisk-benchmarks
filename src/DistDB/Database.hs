{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module DistDB.Database (database) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import DistDB.Utils  
import DistDB.Client (client)
import DistDB.Worker (worker)

remotable [ 'client, 'worker ]

database :: [NodeId] -> [NodeId] -> Process ()
database workerNodes clientNodes = do
  self <- getSelfPid
  workerPids <- spawnSymmetric workerNodes $ $(mkBriskClosure 'worker) ()
  clientPids <- spawnSymmetric clientNodes $ $(mkBriskClosure 'client) self -- 
  loop workerPids
  where
    loop ws = do
      request <- expect :: Process Request
      n <- liftIO $ getChar >>= \ c -> if c == 'a' then return 0 else return 1
      let workerPid = chooseSymmetric ws n
      send workerPid request
      loop ws
