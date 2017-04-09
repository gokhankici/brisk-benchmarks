{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module WorkStealCF.Queue (queue) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (foldM, forM)

import WorkStealCF.Utils
import WorkStealCF.Mapper  

remotable [ 'mapper ]

queue :: [NodeId] -> Process ()
queue nodes =
  do self       <- getSelfPid
     mapperPids <- spawnSymmetric nodes $ $(mkBriskClosure 'mapper) self
     foldM go () [1::Int .. workCount]
     forM mapperPids (\_ -> do (Request pid) <- expect :: Process Request
                               send pid Term)
     return ()
  where
    go _ i = do (Request mapperId) <- expect :: Process Request
                send mapperId (Work i)
