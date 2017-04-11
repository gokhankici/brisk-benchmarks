{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude hiding (FilePath)
import           Turtle hiding (Row)
import qualified Data.Text as T
import           Filesystem.Path.CurrentOS (encodeString)
import           System.Console.ANSI
import           Control.Concurrent.ParallelIO.Global
import           Control.Concurrent.Lock         ( Lock )
import qualified Control.Concurrent.Lock as Lock ( new, with )
import           Control.Exception
import           Control.Monad
import qualified Control.Foldl as L
import qualified Text.Printf as P
import System.Exit
import qualified Control.Foldl as F
import Data.Char (isSpace)
import Data.Maybe (isJust)
-- -----------------------------------------------------------------------------
-- ARGUMENTS
-- -----------------------------------------------------------------------------

bmkParser :: Parser FilePath
bmkParser = optPath "file" 'f' "Name of the benchmark file"

binderParser :: Parser Text
binderParser = optText "binder" 'b' "Name of the binder"

plParser :: Parser Bool
plParser = switch  "prolog" 'p' "Emit prolog"

tableParser :: Parser Bool
tableParser = switch  "table" 't' "Print latex benchmarks table"

negtableParser :: Parser Bool
negtableParser = switch  "negtable" 'n' "Print latex negative bmks table"

data Options = Options { inputs   :: [Input]
                       , isPL     :: Bool
                       , isTbl    :: Bool
                       , isNegTbl :: Bool
                       }

parser :: Parser Options
parser = Options <$> (many $ (,) <$> bmkParser <*> binderParser)
                 <*> plParser
                 <*> tableParser
                 <*> negtableParser

type Input = (FilePath, Text)

defaultArgs :: [Input]
defaultArgs = [ ("src/AsyncP/Master.hs", "master")
              , ("src/ConcDB/Database.hs", "database")
              , ("src/DistDB/Database.hs", "database")
              , ("src/Firewall/Master.hs", "master")
              , ("src/LockServer/Master.hs", "master")
              , ("src/MapReduce/Master.hs", "master")
              , ("src/MultiPing/Master.hs", "master")
              , ("src/Parikh/Master.hs", "master")
              , ("src/PingDet/Master.hs", "master")
              , ("src/PingIter/Master.hs", "master")
              , ("src/PingSym/Master.hs", "master")
              , ("src/PingSym2/Master.hs", "master")
              , ("src/Registry/Master.hs", "master")
              , ("src/TwoBuyers/Master.hs", "master")
              , ("src/TwoPhaseCommit/TwoPhaseCommit.hs", "main")
              , ("src/WorkSteal/Queue.hs", "queue")
              ]

nameMapping :: [(FilePath, String)]
nameMapping = [ ("AsyncP"         , "\\AsyncPing")
              , ("ConcDB"         , "\\concdb")
              , ("DistDB"         , "\\distdb")
              , ("Firewall"       , "\\firewall")
              , ("LockServer"     , "\\lockserver")
              , ("MapReduce"      , "\\mapreduce")
              , ("MultiPing"      , "\\MultiPing")
              , ("Parikh"         , "\\parikh")
              , ("PingDet"        , "\\PingDet")
              , ("PingIter"       , "\\PingIter")
              , ("PingSym"        , "\\PingSym")
              , ("PingSym2"       , "\\PingSymTwo")
              , ("Registry"       , "\\registry")
              , ("TwoBuyers"      , "\\twobuyers")
              , ("TwoPhaseCommit" , "\\twophasecommit")
              , ("WorkSteal"      , "\\ws")
              ]

negArgs :: [(FilePath, Text, String)]
negArgs = [ ("src/AsyncPWrongType/Master.hs", "master", "\\pingmultiWrongType")
          , ("src/AsyncPWithRace/Master.hs", "master", "\\pingmultiWithRace")
          , ("src/MapReduceNoWork/Master.hs", "master", "\\mapreduceNoWork")
          , ("src/MapReduceNoTerm/Master.hs", "master", "\\mapreduceNoTerm")
          , ("src/MapReduceCF/Master.hs", "master", "\\mapreduceCF")
          , ("src/MapReduceNoMaster/Master.hs", "master", "\\mapreduceNoMaster")
          , ("src/MapReduceNoReduce/Master.hs", "master", "\\mapreduceNoReduce")
          , ("src/FirewallWrongPid/Master.hs", "master", "\\firewallWrongPid")
          , ("src/WorkStealCF/Queue.hs", "queue", "\\wsCF")
          ]

data Spin = SpinRow  { s_name  :: String     -- name
                     , s_minN  :: Int        -- min N to fail
                     , s_state :: Double     -- max # of states reached before failing
                     , s_mem   :: Double     -- memory used to store those states
                     , s_time  :: Double     -- timestamp before failure
                     }
          -- able to check N > 100
          | Infty    { s_name :: String     -- name 
                     }

spinResults :: [Spin]
spinResults = [ (SpinRow "AsyncP" 11 1.6e7 4974.259 56.6)
              , (SpinRow "ConcDB" 6 1.0e7 7512.058 52.6)
              , (SpinRow "DistDB" 2 2.1e7 7673.966 46.2)
              , (SpinRow "Firewall" 9 1.7e7 5140.47 55.6)
              , (SpinRow "LockServer" 12 1.5e7 4828.458 55.8)
              , (SpinRow "MapReduce" 4 2.1e7 7507.657 44.8)
              , (Infty "MultiPing")
              , (Infty "Parikh")
              , (SpinRow "PingDet" 13 1.0e7 3757.267 56.3)
              , (SpinRow "PingIter" 11 1.5e7 4671.134 55.2)
              , (SpinRow "PingSym" 10 1.6e7 4108.438 55.1)
              , (SpinRow "PingSym2" 7 2.4e7 6343.888 58.5)
              , (SpinRow "Registry" 10 2.2e7 7570.157 53.0)
              , (Infty "TwoBuyers")
              , (SpinRow "TwoPhaseCommit" 6 1.8e7 7679.923 40.0)
              , (SpinRow "WorkSteal" 5 2.5e7 7700.724 44.9)
              ]

-- -----------------------------------------------------------------------------
-- TESTING
-- -----------------------------------------------------------------------------

runBenchmark :: Lock -> Input -> IO ()
runBenchmark lock (fn,n) = do
  (rc, out, _) <- procStrictWithErr
                    "stack" [ "exec", "--"
                            , "brisk"
                            , "--file", fromPath fn
                            , "--binder", n
                            ] empty

  case rc of
    ExitSuccess   -> Lock.with lock $
                     success $ "[SUCCESS] " <++> fromPath (dirname fn) <++> " - " <++> n
    ExitFailure _ -> Lock.with lock $ do
      failure $ "[ERROR]   " <++> fromPath (dirname fn) <++> " - " <++> n
      normal $ T.dropWhileEnd (== '\n') $ T.replace "\x1b[1;31m" "" out
  return ()

emitProlog :: Lock -> FilePath -> Text -> IO ()
emitProlog lock fn n = do
  (rc, out, _) <- procStrictWithErr
                    "stack" [ "ghc", "--"
                            , "-fforce-recomp"
                            , "--make", "-i./src"
                            , "-fplugin", "Brisk.Plugin"
                            , "-fplugin-opt", "Brisk.Plugin:" <++> n
                            , fromPath fn
                            ] empty
  case rc of
    ExitSuccess   -> Lock.with lock $ do
      info $ "[PROLOG] " <++> fromPath (dirname fn) <++> " - " <++> n
      normal out
    ExitFailure _ -> Lock.with lock $ do
      failure $ "[ERROR]  " <++> fromPath (dirname fn) <++> " - " <++> n
      normal out
  return ()

-- -----------------------------------------------------------------------------
-- TABLE
-- -----------------------------------------------------------------------------

printTableLine :: (Input, (FilePath, String), Spin) -> IO ()
printTableLine ((fn, binder), (name, latexCmd), spin) = do
  let bmk = (encodeString $ dirname fn)
  assert (encodeString name == s_name spin) (return ())
  assert (bmk == s_name spin) (return ())
  
  let fldr = "src" </> name
  noOfLines <- countHSLines fldr

  let maxProcCount = case spin of
                       SpinRow{..} -> show s_minN
                       Infty{..}   -> "-" :: String
  let spinRuntime = case spin of
                       SpinRow{..} -> show s_time
                       Infty{..}   -> "-" :: String
  let spinMem = case spin of
                  SpinRow{..} -> P.printf "%.1g" (s_mem / 1000)
                  Infty{..}   -> "-" :: String

  (rc, toolRuntime) <- getToolRuntime fn binder
  case rc of
    ExitSuccess   -> return ()
    ExitFailure _ -> putStrLn bmk >> fail "neg bmk failed"

  let isSym = case spin of
                Infty{..} -> "" :: String
                _         -> "\\tck"

  P.printf "%s & %s & %d & %s & %s & %s & %d \\\\\n"
    latexCmd isSym noOfLines maxProcCount spinMem spinRuntime toolRuntime

printNegTableLine :: (FilePath, Text, String) -> IO ()
printNegTableLine (fn, binder, latexCmd) = do
  let bmkName   = dirname fn
      bmkFolder = parent fn

  noOfLines <- countHSLines bmkFolder

  (rc, toolRuntime) <- getToolRuntime fn binder

  case rc of
    ExitSuccess   -> putStrLn (encodeString bmkName) >> fail "neg bmk failed"
    ExitFailure _ -> return ()
    
  P.printf "%s & %d & %d \\\\\n" latexCmd toolRuntime noOfLines

runtimeOutputParser :: Pattern Int
runtimeOutputParser = text "rewrite in:" *> spaces *> decimal <* text "ms"

countHSLines :: FilePath -> IO Int
countHSLines fn = do
  hsFiles <- fold (ls fn) $ filter' (\f' -> maybe False (== "hs") (extension f'))
  let hslines = cat $ map input hsFiles
  fold (isLine <$> hslines) F.sum
  where
    isLine :: Line -> Int
    isLine l = let t       = lineToText l
               in if isNonEmptyLine t && isNonOmittedLine t
                  then 1
                  else 0

isNonOmittedLine :: Text -> Bool
isNonOmittedLine t = let ll = T.breakOnAll <$> [ "import"
                                               , "LANGUAGE"
                                               , "module"
                                               ] <*> [t]
                     in foldr (\l b -> b && length l == 0) True ll

isNonEmptyLine :: Text -> Bool
isNonEmptyLine t = T.length t > 0 && isJust (T.findIndex (not . isSpace) t)

getToolRuntime :: FilePath -> Text -> IO (ExitCode, Int)
getToolRuntime fn binder = do
  (rc, out, _) <- procStrictWithErr
                    "stack" [ "exec", "--"
                            , "brisk"
                            , "--file", fromPath fn
                            , "--binder", binder
                            ] empty
  let rs = concatMap (match runtimeOutputParser) (T.lines out)
  case rs of
    []  -> putStrLn (encodeString $ dirname fn) >> fail "neg bmk failed"
    h:_ -> return (rc,h)
  
-- -----------------------------------------------------------------------------
-- MAIN
-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  Options{..} <- options "Runs brisk benchmarks" parser

  let args' = case inputs of
                [] -> defaultArgs
                _  -> inputs
  stdoutLock <- Lock.new

  when isTbl $ do
    assert (length nameMapping == length spinResults) (return ())
    forM_ (zip3 defaultArgs nameMapping spinResults) printTableLine
    exitSuccess

  when isNegTbl $ do
    forM_ negArgs printNegTableLine
    exitSuccess

  do _ <- parallelInterleaved $
            if isPL
            then [ emitProlog stdoutLock fn n | (fn,n) <- args' ]
            else [ runBenchmark stdoutLock a | a <- args' ]
     stopGlobalPool

-- -----------------------------------------------------------------------------
-- HELPER FUNCTIONS
-- -----------------------------------------------------------------------------

(<++>) :: Text -> Text -> Text
(<++>) = T.append

fromPath :: FilePath -> Text
fromPath = T.pack . encodeString

normal, info, success, failure :: Text -> IO ()
normal  = putStrLn . T.unpack
info    = colored Blue
success = colored Green
failure = colored Red

colored :: Color -> Text -> IO ()
colored c t = do
  setSGR [ SetColor Foreground Vivid c
         , SetConsoleIntensity BoldIntensity
         ]
  putStrLn $ T.unpack t
  setSGR [Reset]

test :: (MonadIO io) => io Bool -> io ExitCode
test action = do
  res <- action
  return $ if res
           then ExitSuccess
           else ExitFailure 1

rmf :: (MonadIO io) => FilePath -> io ()
rmf fn = do fnExists <- testfile fn
            if fnExists
              then rm fn
              else return ()

mkdirp :: (MonadIO io) => FilePath -> io ()
mkdirp fn = do fnExists <- testdir fn
               if not fnExists
                 then mkdir fn
                 else return ()

mergeOutputs :: Shell (Either Line Line) -> Shell Line
mergeOutputs = fmap (either id id)

filter' :: (a -> Bool) -> Fold a [a]
filter' p = L.foldMap (\a -> if p a then [a] else []) id
