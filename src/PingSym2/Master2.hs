{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingSym2.Master2 (master2) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM, foldM)

import PingSym2.Utils

master2 :: [NodeId] -> Process ()
master2 ns = do
  foldM (\_ _ -> do expect :: Process Master2Msg
                    return ()) () ns
  return ()
