{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module MapReduce.Mapper (mapper) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import MapReduce.Utils  

mapper :: (ProcessId, ProcessId) -> Process ()
mapper (queue, master) = loop
  where loop = 
          do self <- getSelfPid
             -- request a work from the work queue
             send queue (Request self)
             -- block until receive the work
             req <- expect :: Process Work
             case req of
               -- if got a work, send the processed
               -- result to the master process
               Work i -> do send master (Result i)
                            loop
               -- otherwise, there must be no more work
               -- mapper is shutdown
               Term   -> return ()
