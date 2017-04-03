{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ConcDB.Database (database) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import ConcDB.Utils  
import ConcDB.Client (client)

remotable [ 'client ]

database :: [NodeId] -> Process ()
database nodes = do
  self <- getSelfPid
  clientPids <- spawnSymmetric nodes $ $(mkBriskClosure 'client) self
  loop
  where
    loop = do request <- expect :: Process Request
              case request of
                Allocate pid key -> do
                  allocated <- liftIO $ getChar >>= \c ->
                    if (c == 'a')
                    then return True
                    else return False

                  if allocated
                    then send pid Allocated
                    else do send pid Free
                            expectFrom pid :: Process SetRequest
                            return ()

                Lookup   pid key -> do
                  let result = Value "bar"
                  send pid result
              loop
