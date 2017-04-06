#!/usr/bin/env stack
{- stack --resolver lts-8.3 --install-ghc runghc
   --package turtle
   --package system-filepath
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (FilePath)
import Turtle
import qualified Data.Text as T (pack)
import Filesystem.Path.CurrentOS (encodeString)
import Control.Monad
import Control.Exception
import qualified Text.Printf as TP

benchmarks = [ "AsyncP/asyncp.pml"
             -- , "ConcDB/concdb.pml"
             -- , "LockServer/lockserver.pml"
             -- , "MapReduce/mapreduce.pml"
             -- , "Registry/registry.pml"
             -- , "TwoPhaseCommit/twophasecommit.pml"
             -- , "WorkStealing/workstealing.pml"
             ]

spinArgs = [ "-run", "-safety" ]

main :: IO ()
main = forM_ benchmarks (getMaxCount 1)

getMaxCount :: Int -> FilePath -> IO ()
getMaxCount n f = do
  fExists <- testfile f
  (r, out, _) <- procStrictWithErr
                   "./nspin" ([ T.pack $ show n
                              , T.pack $ encodeString f
                              ] ++ spinArgs) empty
  case r of
    ExitSuccess    -> do getMaxCount (n+1) f
                         TP.printf "[PASS] %20s : %d" (encodeString $ dirname f) n
    ExitFailure rc -> TP.printf "[FAIL] %20s : %d" (encodeString $ dirname f) n
  assert fExists return ()
  return ()
