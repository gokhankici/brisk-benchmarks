#!/usr/bin/env stack
{- stack --resolver lts-8.3 --install-ghc runghc
   --package turtle
   --package system-filepath
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (FilePath)
import Turtle
import qualified Data.Text as T (pack, unpack)
import Filesystem.Path.CurrentOS (encodeString)
import Control.Monad
import Control.Exception
import qualified Text.Printf as TP

benchmarks = [ "AsyncP/asyncp.pml"
             , "ConcDB/concdb.pml"
             , "DistDB/distdb.pml"
             , "Firewall/firewall.pml"
             , "LockServer/lockserver.pml"
             , "MapReduce/mapreduce.pml"
             , "MultiPing/multiping.pml"
             , "Parikh/parikh.pml"
             , "PingDet/pingdet.pml"
             , "PingIter/pingiter.pml"
             , "PingSym2/pingsym2.pml"
             , "PingSym/pingsym.pml"
             , "Registry/registry.pml"
             , "TwoBuyers/twobuyers.pml"
             , "TwoPhaseCommit/twophasecommit.pml"
             , "WorkStealing/workstealing.pml"
             ]

defaultMemoryLimit = "2000000"
defaultTimeLimit   = "90"
spinArgs           = [ "-run", "-safety" ]

limitParser :: Parser Text
limitParser = optText "memorylimit" 'm' "Limit the memory usage (in Ks) of spin (default: 2000000K)"

bmkParser :: Parser FilePath
bmkParser = optPath "benchmark" 'b' "Promela benchmark (default: every benchmark)"

timeParser :: Parser Text
timeParser = optText "timelimit" 't' "Limit the time (in secs) of spin (default: 90s)"

main :: IO ()
main = do
  (m,bmk,t) <- options "Run promela benchmarks" $ (,,) <$> optional limitParser
                                                       <*> optional bmkParser
                                                       <*> optional timeParser

  let memoryLimit = case m of
                      Nothing -> defaultMemoryLimit
                      Just m' -> m'

  let timeLimit = case t of
                    Nothing -> defaultTimeLimit
                    Just m' -> m'

  let timeoutArgs = [ "-m", memoryLimit
                    , "-t", timeLimit
                    ]

  let bmks = case bmk of
               Nothing   -> benchmarks
               Just bmk' -> [bmk']

  TP.printf "Memory limit: %sK\n" (T.unpack memoryLimit)
  TP.printf "Time limit:   %s secs\n\n" (T.unpack timeLimit)

  forM_ bmks (getMaxCount 1 timeoutArgs)

getMaxCount :: Int -> [Text] -> FilePath -> IO ()
getMaxCount n timeoutArgs f = do
  fExists <- testfile f
  -- https://github.com/pshved/timeout
  (r, out, _) <- procStrictWithErr
                   "./timeout" ([ "--confess" ] -- return non zero if mem limit is exceeded
                                ++ timeoutArgs ++
                                [ "./nspin"
                                , T.pack $ show n
                                , T.pack $ encodeString f
                                ] ++ spinArgs) empty
  case r of
    ExitSuccess    -> do -- TP.printf "[PASS] %-15s : %d\n" (encodeString $ dirname f) n
                         getMaxCount (n+1) timeoutArgs f
    ExitFailure rc -> TP.printf "%s %d\n" (encodeString $ dirname f) n
  assert fExists return ()
  return ()
