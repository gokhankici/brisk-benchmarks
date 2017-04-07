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
spinArgs           = [ "-run", "-safety", "-m100000" ]

limitParser :: Parser Text
limitParser = optText "memorylimit" 'm' "Limit the memory usage (in Ks) of spin (default: 2000000K)"

bmkParser :: Parser FilePath
bmkParser = optPath "benchmark" 'b' "Promela benchmark (default: every benchmark)"

timeParser :: Parser Text
timeParser = optText "timelimit" 't' "Limit the time (in secs) of spin (default: 90s)"

main :: IO ()
main = do
  (m,bmks',t) <- options "Run promela benchmarks" $ (,,) <$> optional limitParser
                                                       <*> many bmkParser
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

  let bmks = case bmks' of
               [] -> benchmarks
               _  -> bmks'

  TP.printf "Memory limit: %sK\n" (T.unpack memoryLimit)
  TP.printf "Time limit:   %s secs\n\n" (T.unpack timeLimit)

  forM_ bmks (\f -> do
                 fExists <- testfile f
                 assert fExists return ())

  forM_ bmks (getMaxCount 0 1 infty timeoutArgs) -- uhhh

infty = 128

getMaxCount :: Int -> Int -> Int -> [Text] -> FilePath -> IO ()
getMaxCount maxSuccess toCheck minFail timeoutArgs f = do
  -- https://github.com/pshved/timeout
  (r, out, _) <- procStrictWithErr
                   "./timeout" ([ "--confess" ] -- return non zero if mem limit is exceeded
                                ++ timeoutArgs ++
                                [ "./nspin"
                                , T.pack $ show toCheck
                                , T.pack $ encodeString f
                                ] ++ spinArgs) empty
  case r of
    ExitSuccess    -> if   toCheck > 100
                      then TP.printf "[DONE]      %-15s : %d\n" (encodeString $ dirname f) toCheck
                      else do TP.printf "[PASS]      %-15s : %d\n" (encodeString $ dirname f) toCheck
                              if minFail == infty
                                then getMaxCount toCheck (toCheck * 2) minFail timeoutArgs f
                                else do let toCheck'    = (toCheck + minFail) `div` 2
                                            maxSuccess' = toCheck
                                            minFail'    = minFail
                                        when (toCheck' < minFail' + 1 && maxSuccess' < toCheck') $
                                          getMaxCount maxSuccess' toCheck' minFail' timeoutArgs f
    ExitFailure rc -> do if   rc > 128
                           then TP.printf "[FAIL]      %-15s : %d\n" (encodeString $ dirname f) toCheck
                           else TP.printf "[SPIN FAIL] %-15s : %d\n" (encodeString $ dirname f) toCheck
                         let toCheck'    = (toCheck + maxSuccess) `div` 2
                             minFail'    = toCheck
                             maxSuccess' = maxSuccess
                         when (toCheck' < minFail' + 1 && maxSuccess' < toCheck') $
                           getMaxCount maxSuccess toCheck' minFail' timeoutArgs f
