{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ConcDB.Client (client) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import ConcDB.Utils  

client :: ProcessId -> Process ()
client db = loop
  where
    loop = do
      me <- getSelfPid
      choice <- liftIO $ getChar >>= \c -> return (c == 'a')
      msg <- if choice then return $ Allocate me "foo" 
                       else return $ Lookup me "foo"
      send db msg
      if choice then do
         response <- expect :: Process AllocateResponse
         case response of
           Allocated -> return ()
           Free      -> send db (SetValue "bar")
      else do
         expect :: Process LookupResponse
         return ()
      loop
