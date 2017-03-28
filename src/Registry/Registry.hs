{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Registry.Registry (registry) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import Control.Monad (forM, foldM)

import Registry.Utils  

registry :: Int -> Process ()
registry n = do (MasterMsg master) <- expect
                foldM (\_ _ -> do
                          (ClientMsg _c) <- expect
                          return ()) () [1..n]
                send master ()
                return ()
