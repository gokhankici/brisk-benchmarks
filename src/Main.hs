{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Turtle
import Data.Text as T (pack, append)
import Filesystem.Path.CurrentOS (encodeString)

-- ARGUMENTS

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
defaultArgs = [ ("src/MapReduce/Master.hs", "master") ]

-- TESTING

runBenchmarks :: (MonadIO io) => [Input] -> io ()
runBenchmarks _ = return ()

runBenchmark :: (MonadIO io) => Input -> io ExitCode
runBenchmark (fn,n) = do
 proc "stack" [ "exec", "--"
              , "brisk"
              , "--file", fromPath fn
              , "--binder", n
              ] empty 

emitProlog :: (MonadIO io) => FilePath -> Text -> io ExitCode
emitProlog fn n = proc "stack" [ "ghc", "--"
                               , "-fforce-recomp"
                               , "--make", "-i./src"
                               , "-fplugin", "Brisk.Plugin"
                               , "-fplugin-opt", "Brisk.Plugin:" <++> n
                               , fromPath fn
                               ] empty
                                
-- MAIN

main :: IO ()
main = do
  (args,emitPl) <- options "Runs brisk benchmarks" parser
  if emitPl
    then case args of
           []       -> die "No benchmark given"
           (fn,n):_ -> do rc <- emitProlog fn n
                          exit rc
    else case args of
           [] -> runBenchmarks defaultArgs
           _  -> runBenchmarks args
  

-- HELPER FUNCTIONS

(<++>) :: Text -> Text -> Text
(<++>) = T.append

fromPath :: FilePath -> Text
fromPath = T.pack . encodeString
