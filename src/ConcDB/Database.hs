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
  _clientPids <- spawnSymmetric nodes $ $(mkBriskClosure 'client) self
  loop
  where
    loop = do request <- expect :: Process Request
              case request of
                Allocate pid _key -> do
                  allocated <- liftIO $ getChar >>= \c ->
                    if (c == 'a')
                    then return True
                    else return False

                  if allocated
                    then send pid Allocated
                    else do send pid Free
                            _ <- expectFrom pid :: Process SetRequest
                            return ()

                Lookup   pid _key -> do
                  choice <- liftIO $ getChar >>= \c ->
                    if (c == 'a')
                    then return Allocated
                    else return Free
                  send pid choice
              loop
