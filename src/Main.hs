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
import           Control.Monad
import qualified Control.Foldl as L
import qualified Text.Printf as P
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

data Options = Options { inputs :: [Input] }

parser :: Parser Options
parser = Options <$> (many $ (,) <$> bmkParser <*> binderParser)

type Input = (FilePath, Text)

defaultArgs :: [Input]
defaultArgs = [ ("src/MultiPing/Master.hs"              , "master")
              , ("src/AsyncP/Master.hs"                 , "master")
              , ("src/PingDet/Master.hs"                , "master")
              , ("src/PingIter/Master.hs"               , "master")
              , ("src/PingSym/Master.hs"                , "master")
              , ("src/PingSym2/Master.hs"               , "master")
              , ("src/ConcDB/Database.hs"               , "database")
              , ("src/DistDB/Database.hs"               , "database")
              , ("src/Firewall/Master.hs"               , "master")
              , ("src/LockServer/Master.hs"             , "master")
              , ("src/MapReduce/Master.hs"              , "master")
              , ("src/Parikh/Master.hs"                 , "master")
              , ("src/Registry/Master.hs"               , "master")
              , ("src/TwoBuyers/Master.hs"              , "master")
              , ("src/TwoPhaseCommit/TwoPhaseCommit.hs" , "main")
              , ("src/WorkSteal/Queue.hs"               , "queue")
              ]

negArgs :: [Input]
negArgs = [ ("src/AsyncPWrongType/Master.hs"   , "master")
          , ("src/AsyncPWithRace/Master.hs"    , "master")
          , ("src/MapReduceNoWork/Master.hs"   , "master")
          , ("src/MapReduceNoTerm/Master.hs"   , "master")
          , ("src/MapReduceCF/Master.hs"       , "master")
          , ("src/MapReduceNoMaster/Master.hs" , "master")
          , ("src/MapReduceNoReduce/Master.hs" , "master")
          , ("src/FirewallWrongPid/Master.hs"  , "master")
          , ("src/WorkStealCF/Queue.hs"        , "queue")
          ]


-- -----------------------------------------------------------------------------
-- TESTING
-- -----------------------------------------------------------------------------

runBenchmark :: Lock -> Input -> IO ()
runBenchmark lock (fn, binder) = do
  (rc, rewrite, real) <- getToolRuntime fn binder

  case rc of
    ExitSuccess   -> Lock.with lock $
      success $ "[SUCCESS] " <++> resultToStr fn rewrite real
    ExitFailure _ -> Lock.with lock $ do
      failure $ "[ERROR]   " <++> resultToStr fn rewrite real
  return ()

resultToStr :: FilePath -> Int -> Double -> Text
resultToStr fn t1 t2 =
  let t2' :: Int = (round (t2 * 10)) * 100 in
  T.pack $ P.printf "%-20s   %4d ms   %4d ms" (encodeString (dirname fn)) t1 t2'

runThequeFs :: Lock -> IO ()
runThequeFs lock = do
  ((rc, out, _), tdiff) <- time $ runInDirectory "/home/paper34/Desktop/artifact/thequefs" $ procStrictWithErr
                             "stack" [ "exec", "--"
                                     , "brisk"
                                     , "--file", "app/Scenario.hs"
                                     , "--binder", "main"
                                     ] empty
  let fn        = "../ThequeFS/app"
      rewrite:_ = concatMap (match runtimeOutputParser) (T.lines out)
      real      = realToFrac tdiff
      
  case rc of
    ExitSuccess   -> Lock.with lock $
      success $ "[SUCCESS] " <++> resultToStr fn rewrite real
    ExitFailure _ -> Lock.with lock $ do
      failure $ "[ERROR]   " <++> resultToStr fn rewrite real

  return ()

runInDirectory :: FilePath -> IO a -> IO a
runInDirectory f io = with (pushd f) $ \_ -> io

-- -----------------------------------------------------------------------------
-- MAIN
-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  Options{..} <- options "Runs brisk benchmarks" parser

  stdoutLock <- Lock.new

  let hd1 = "Benchmark Name" :: String
      hd2 = "Rewrite" :: String
      hd3 = "Total" :: String

  case inputs of
    [] -> do P.printf "---- Positive Benchmarks -------------------------\n"
             P.printf "          %-20s   %-7s   %-7s\n" hd1 hd2 hd3
             P.printf "--------------------------------------------------\n"
             forM_ defaultArgs (runBenchmark stdoutLock)
             runThequeFs stdoutLock
             P.printf "\n---- Negative Benchmarks -------------------------\n"
             P.printf "          %-20s   %-7s   %-7s\n" hd1 hd2 hd3
             P.printf "--------------------------------------------------\n"
             forM_ negArgs     (runBenchmark stdoutLock)
    _  -> forM_ inputs (runBenchmark stdoutLock) 

  stopGlobalPool

-- -----------------------------------------------------------------------------
-- HELPER FUNCTIONS
-- -----------------------------------------------------------------------------

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

getToolRuntime :: FilePath -> Text -> IO (ExitCode, Int, Double)
getToolRuntime fn binder = do
  ((rc, out, _), tdiff) <- time $ procStrictWithErr
                             "stack" [ "exec", "--"
                                     , "brisk"
                                     , "--file", fromPath fn
                                     , "--binder", binder
                                     ] empty

  let rs = concatMap (match runtimeOutputParser) (T.lines out)
  case rs of
    []  -> putStrLn (encodeString $ dirname fn) >> fail "tool runtime failed"
    h:_ -> return (rc,h, realToFrac tdiff)
 

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

