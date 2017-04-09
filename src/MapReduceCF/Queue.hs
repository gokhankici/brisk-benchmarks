{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module MapReduceCF.Queue (queue) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (foldM, forM)

import MapReduceCF.Utils
import MapReduceCF.Mapper  

remotable [ 'mapper ]

queue :: ([NodeId], [Int], ProcessId) -> Process ()
queue (nodes, work, m) =
  do self <- getSelfPid

     -- create the workers
     mapperPids <- spawnSymmetric nodes $ $(mkBriskClosure 'mapper) (self, m)

     -- for k times ...
     foldM go () work
  
     -- for each mapper ...
     forM mapperPids (\x -> do (Request pid) <- expect
                               send pid Term)

     return ()
  where
    go _ i = do (Request mapperId) <- expect :: Process Request
                send mapperId (Work i)
