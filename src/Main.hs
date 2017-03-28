{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Turtle
import qualified Data.Text as T (pack, unpack, append)
import Filesystem.Path.CurrentOS (encodeString)
import Control.Monad
import System.Console.ANSI
import Control.Concurrent.ParallelIO.Global

-- -----------------------------------------------------------------------------
-- ARGUMENTS
-- -----------------------------------------------------------------------------

runsFolder :: FilePath
runsFolder = "runs"

bmkParser :: Parser FilePath
bmkParser = optPath "benchmark" 'b' "Name of the benchmark file"

binderParser :: Parser Text
binderParser = optText "binder" 'n' "Name of the binder"

plParser :: Parser Bool
plParser = switch  "prolog" 'p' "Emit prolog"

type Input = (FilePath, Text)

parser :: Parser ([Input], Bool)
parser = (,) <$> (many $ (,) <$> bmkParser <*> binderParser)
             <*> plParser

defaultArgs :: [Input]
defaultArgs = [ ("src/MapReduce/Master.hs", "master")
              , ("src/AsyncP/Master.hs", "master")
              , ("src/PingDet/Master.hs", "master")
              , ("src/PingIter/Master.hs", "master")
              , ("src/PingSym/Master.hs", "master")
              , ("src/MultiPing/Master.hs", "master")
              , ("src/ConcDB/Database.hs", "database")
              , ("src/DistDB/Database.hs", "database")
              ]

-- -----------------------------------------------------------------------------
-- TESTING
-- -----------------------------------------------------------------------------

runBenchmarks :: [Input] -> IO ()
runBenchmarks args = forM_ args $ \_arg -> runBenchmark _arg >> echo ""

runBenchmark :: Input -> IO ()
runBenchmark (fn,n) = do
  info $ fromPath (dirname fn) <++> " - " <++> n <++> " [BRISK]"
  sh $ do
    let outputFile = runsFolder </> dirname fn <.> "run"
    rmf outputFile
    append outputFile $ mergeOutputs $
      inprocWithErr "stack" [ "exec", "--"
                            , "brisk"
                            , "--file", fromPath fn
                            , "--binder", n
                            ] empty 
  
  success $ fromPath (dirname fn) <++> " - " <++> n <++> " (DONE)"
  return ()

emitProlog :: FilePath -> Text -> IO ()
emitProlog fn n = do
  info $ fromPath (dirname fn) <++> " - " <++> n <++> " [PROLOG]"
  sh $ do
    let outputFile = runsFolder </> dirname fn <.> "run.pl"
    rmf outputFile
    append outputFile $ mergeOutputs $
      inprocWithErr "stack" [ "ghc", "--"
                            , "-fforce-recomp"
                            , "--make", "-i./src"
                            , "-fplugin", "Brisk.Plugin"
                            , "-fplugin-opt", "Brisk.Plugin:" <++> n
                            , fromPath fn
                            ] empty
  success $ fromPath (dirname fn) <++> " - " <++> n <++> " (DONE)"
  return ()

-- -----------------------------------------------------------------------------
-- MAIN
-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  (args,emitPl) <- options "Runs brisk benchmarks" parser
  let args' = case args of
                [] -> defaultArgs
                _  -> args

  mkdirp runsFolder

  _ <- parallelInterleaved $
    if emitPl
      then [ emitProlog fn n | (fn,n) <- args' ]
      else [ runBenchmark a | a <- args' ]

  case args' of
    [(fn,_)] -> do let out = runsFolder </> dirname fn <.> if emitPl then "run.pl" else "run"
                   stdout $ input out
    _        -> return ()

  stopGlobalPool

-- -----------------------------------------------------------------------------
-- HELPER FUNCTIONS
-- -----------------------------------------------------------------------------

(<++>) :: Text -> Text -> Text
(<++>) = T.append

fromPath :: FilePath -> Text
fromPath = T.pack . encodeString

info, success, failure :: Text -> IO ()
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
