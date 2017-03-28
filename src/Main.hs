{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Turtle
import qualified Data.Text as T (pack, unpack, append)
import Filesystem.Path.CurrentOS (encodeString)
import Control.Monad
import System.Console.ANSI

-- ARGUMENTS

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
              ,( "src/AsyncP/Master.hs", "master")
              ]

-- TESTING

type Output = Either Line Line

runBenchmarks :: [Input] -> IO ()
runBenchmarks args = forM_ args $ \_arg -> runBenchmark _arg >> echo ""

runBenchmark :: Input -> IO ()
runBenchmark (fn,n) = do
  info $ fromPath (dirname fn) <++> " - " <++> n
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
  info $ fromPath (dirname fn) <++> " - " <++> n
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

                                
-- MAIN

main :: IO ()
main = do
  (args,emitPl) <- options "Runs brisk benchmarks" parser
  mkdirp runsFolder
  let args' = case args of
                [] -> defaultArgs
                _  -> args
  if emitPl
    then sh $ parallel [ emitProlog fn n | (fn,n) <- args' ]
    else sh $ parallel [ runBenchmark arg | arg <- args' ]

-- HELPER FUNCTIONS

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
  putStr $ T.unpack t
  setSGR [Reset]
  putStrLn ""

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
