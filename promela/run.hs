#!/usr/bin/env stack
{- stack --resolver lts-8.3 --install-ghc runghc
   --package turtle
   --package parsec
   --package system-filepath
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (FilePath)
import Turtle
import qualified Data.Text as T (pack, unpack, lines)
import Filesystem.Path.CurrentOS (encodeString)
import Control.Monad
import Control.Exception
import qualified Text.Printf as TP
-- import qualified Text.Parsec.String as P
-- import Text.Parsec.Char

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
defaultTimeLimit   = "60"
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

  forM_ bmks (getMaxCount 0 1 (infty,"","") timeoutArgs)

infty = 128

getMaxCount :: Int -> Int -> (Int,Text,Text) -> [Text] -> FilePath -> IO ()
getMaxCount maxSuccess toCheck oldData@(minFail,out',err') timeoutArgs f = do
  -- https://github.com/pshved/timeout
  (r, out, err) <- procStrictWithErr
                     "./timeout" ([ "--confess" ] -- return non zero if mem limit is exceeded
                                  ++ timeoutArgs ++
                                  [ "./nspin"
                                  , T.pack $ show toCheck
                                  , T.pack $ encodeString f
                                  ] ++ spinArgs) empty

  case r of
    ExitSuccess    -> if   toCheck > 100
                      then printDone f
                      else do TP.printf "[PASS]      %-15s : %d\n" (encodeString $ dirname f) toCheck
                              if minFail == infty
                                then getMaxCount toCheck (toCheck * 2) oldData timeoutArgs f
                                else do let toCheck'    = (toCheck + minFail) `div` 2
                                            maxSuccess' = toCheck
                                            minFail'    = minFail
                                            newData     = oldData
                                        if (toCheck' < minFail' && maxSuccess' < toCheck')
                                          then getMaxCount maxSuccess' toCheck' newData timeoutArgs f
                                          else printMinFail f newData
    ExitFailure rc -> do if   rc > 128
                           then TP.printf "[FAIL]      %-15s : %d\n" (encodeString $ dirname f) toCheck
                           else TP.printf "[SPIN FAIL] %-15s : %d\n" (encodeString $ dirname f) toCheck
                         let toCheck'    = (toCheck + maxSuccess) `div` 2
                             maxSuccess' = maxSuccess
                             minFail'    = toCheck
                             newData     = (minFail', out, err)
                         if (toCheck' < minFail' && maxSuccess' < toCheck')
                           then getMaxCount maxSuccess' toCheck' newData timeoutArgs f
                           else printMinFail f newData

printDone :: FilePath -> IO ()
printDone f = do
  let bmk = (encodeString $ dirname f)
  TP.printf ", (Infty \"%s\")\n" bmk

printMinFail :: FilePath -> (Int, Text, Text) -> IO ()
printMinFail f (minFail, out, err) = do
  let bmk = (encodeString $ dirname f)
      spinrows = concatMap (match spinParser) (T.lines out)
  case spinrows of
    [] -> do putStr $ T.unpack out
             putStr $ T.unpack err
             TP.printf ", (SpinFail \"%s\" %d)\n" bmk minFail
    _  -> do let spinrow = last spinrows
             TP.printf ", (SpinRow \"%s\" %d %e %g %g)\n"
               bmk minFail (s_states spinrow) (s_memory spinrow) (s_time spinrow)

data SpinInfo = SpinInfo { s_depth       :: Int
                         , s_states      :: Double
                         , s_transitions :: Double
                         , s_memory      :: Double
                         , s_time        :: Double
                         , s_r           :: Double
                         }
                deriving (Show)
  
spinParser :: Pattern SpinInfo
spinParser = SpinInfo <$> (text "Depth=" *> spaces *> intParser <* spaces)
                      <*> (text "States=" *> spaces *> doubleParser <* spaces)
                      <*> (text "Transitions=" *> spaces *> doubleParser <* spaces)
                      <*> (text "Memory=" *> spaces *> doubleParser <* spaces)
                      <*> (text "t=" *> spaces *> doubleParser <* spaces)
                      <*> (text "R=" *> spaces *> doubleParser <* spaces)

intParser :: Pattern Int
intParser = decimal

doubleParser :: Pattern Double
doubleParser = rd <$> (some $ oneOf "1234567890+-.e") -- umm ...
  where
    rd :: String -> Double
    rd = read
  
