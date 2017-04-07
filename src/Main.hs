{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import           Prelude hiding (FilePath)
import           Turtle hiding (Row)
import qualified Data.Text as T (pack, unpack, append, replace, dropWhileEnd)
import           Filesystem.Path.CurrentOS (encodeString)
import           System.Console.ANSI
import           Control.Concurrent.ParallelIO.Global
import           Control.Concurrent.Lock         ( Lock )
import qualified Control.Concurrent.Lock as Lock ( new, with )
import           Control.Monad
import qualified Control.Foldl as L
import qualified Text.Printf as P
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
tableParser = switch  "table" 't' "Print latex table"

type Input = (FilePath, Text)

parser :: Parser ([Input], Bool, Bool)
parser = (,,) <$> (many $ (,) <$> bmkParser <*> binderParser)
              <*> plParser
              <*> tableParser


defaultArgs :: [Input]
defaultArgs = [ ("src/MapReduce/Master.hs", "master")
              , ("src/WorkSteal/Queue.hs", "queue")
              , ("src/AsyncP/Master.hs", "master")
              , ("src/PingDet/Master.hs", "master")
              , ("src/PingIter/Master.hs", "master")
              , ("src/PingSym/Master.hs", "master")
              , ("src/PingSym2/Master.hs", "master")
              , ("src/MultiPing/Master.hs", "master")
              , ("src/ConcDB/Database.hs", "database")
              , ("src/DistDB/Database.hs", "database")
              , ("src/Registry/Master.hs", "master")
              , ("src/Firewall/Master.hs", "master")
              , ("src/Parikh/Master.hs", "master")
              , ("src/TwoBuyers/Master.hs", "master")
              , ("src/TwoPhaseCommit/TwoPhaseCommit.hs", "main")
              -- , ("src/Auction/Master.hs", "master")
              , ("src/LockServer/Master.hs", "master")
              ]

data SpinTime = MaxN     Int
              | SpinFail Int
              | Infty           -- more than 100

data Row = Row { name     :: FilePath
               , latexCmd :: String
               -- , spinMaxN :: SpinTime
               -- , briskT   :: Double
               -- , spinT    :: Double
               }

nameMapping = [ Row "AsyncP"         "\\AsyncPing"
              , Row "ConcDB"         "\\concdb"
              , Row "DistDB"         "\\distdb"
              , Row "Firewall"       "\\firewall"
              , Row "LockServer"     "\\lockserver"
              , Row "MapReduce"      "\\mapreduce"
              , Row "MultiPing"      "\\MultiPing"
              , Row "Parikh"         "\\parikh"
              , Row "PingDet"        "\\PingDet"
              , Row "PingIter"       "\\PingIter"
              , Row "PingSym"        "\\PingSym"
              , Row "PingSym2"       "\\PingSymTwo"
              , Row "Registry"       "\\registry"
              , Row "TwoBuyers"      "\\twobuyers"
              , Row "TwoPhaseCommit" "\\twophasecommit"
              , Row "WorkSteal"      "\\ws"
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
-- MAIN
-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  (args,emitPl,tbl) <- options "Runs brisk benchmarks" parser
  let args' = case args of
                [] -> defaultArgs
                _  -> args
  stdoutLock <- Lock.new

  if tbl
    then do putStrLn "\\textbf{Benchmark} & \\textbf{\\#Proc.} & \\textbf{\\spin(s)} & \\textbf{\\Tool(s)} & \\textbf{(LOC)} \\\\"
            putStrLn "\\midrule"
            forM_ nameMapping printTableLine
    else do _ <- parallelInterleaved $
              if emitPl
              then [ emitProlog stdoutLock fn n | (fn,n) <- args' ]
              else [ runBenchmark stdoutLock a | a <- args' ]
            stopGlobalPool

printTableLine :: Row -> IO ()
printTableLine Row{..} = do
  let fldr = "src" </> name
  hsFiles <- fold (ls fldr) $ filter' (\f' -> maybe False (== "hs") (extension f'))
  noOfLines :: Int <- fold (cat $ map input hsFiles) countLines
  let (maxProcCount :: String) = ""
      (spinRuntime :: String) = ""
      (toolRuntime :: String) = ""
  P.printf "%s & %s & %s & %s & %d \\\\\n" latexCmd maxProcCount spinRuntime toolRuntime noOfLines


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
