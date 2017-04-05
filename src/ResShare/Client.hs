{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ResShare.Client (client) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import ResShare.Utils

client :: (SymSet ProcessId) -> Process ()
client servers = loop
  where
    loop = do 
      self <- getSelfPid
      forM servers (\s -> do (selfSign Lock) >>= send s
                             Ack <- expectFrom s
                             (selfSign Unlock) >>= send s)
      loop
