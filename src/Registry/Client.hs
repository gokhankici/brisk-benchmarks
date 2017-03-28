{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Registry.Client (client) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import Registry.Utils  

client :: () -> Process ()
client _ = do (RegistryMsg reg) <- expect
              self <- getSelfPid
              send reg (ClientMsg self)
