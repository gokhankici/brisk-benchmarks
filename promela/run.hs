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
             , "LockServer/lockserver.pml"
             , "MapReduce/mapreduce.pml"
             , "Registry/registry.pml"
             , "TwoPhaseCommit/twophasecommit.pml"
             , "WorkStealing/workstealing.pml"
             ]

defaultMemoryLimit = "2000000"
spinArgs = [ "-run", "-safety" ]

limitParser :: Parser Text
limitParser = optText "memorylimit" 'm' "Limit the memory usage (in Ks) of spin (default: 2000000)"

bmkParser :: Parser FilePath
bmkParser = optPath "benchmark" 'b' "Promela benchmark (default: every benchmark)"

main :: IO ()
main = do
  (m,bmk) <- options "Run promela benchmarks" $ (,) <$> optional limitParser
                                                    <*> optional bmkParser
  let memoryLimit = case m of
                      Nothing -> defaultMemoryLimit
                      Just m' -> m'
  let bmks = case bmk of
               Nothing   -> benchmarks
               Just bmk' -> [bmk']

  TP.printf "Memory limit: %sK\n" (T.unpack memoryLimit)

  forM_ bmks (getMaxCount 1 memoryLimit)

getMaxCount :: Int -> Text -> FilePath -> IO ()
getMaxCount n memoryLimit f = do
  fExists <- testfile f
  -- https://github.com/pshved/timeout
  (r, out, _) <- procStrictWithErr
                   "./timeout" ([ "--confess" -- return non zero if mem limit is exceeded
                                , "-m", memoryLimit
                                , "./nspin"
                                , T.pack $ show n
                                , T.pack $ encodeString f
                                ] ++ spinArgs) empty
  case r of
    ExitSuccess    -> do TP.printf "[PASS] %-15s : %d\n" (encodeString $ dirname f) n
                         getMaxCount (n+1) memoryLimit f
    ExitFailure rc -> TP.printf "[FAIL] %-15s : %d\n" (encodeString $ dirname f) n
  assert fExists return ()
  return ()
