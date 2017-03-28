{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module PingSym2.Master2 (master2) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM, foldM)

master2 :: Int -> Process ()
master2 n = do
  self <- getSelfPid
  foldM (\_ _ -> do
            _p <- expect :: Process ProcessId
            return ()) () [1..n]
  return ()
