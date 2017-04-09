{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module WorkStealCF.Mapper (mapper) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess

import WorkStealCF.Utils  

mapper :: ProcessId -> Process ()
mapper queue = loop
  where loop = 
          do self <- getSelfPid
             -- request a work from the work queue
             send queue (Request self)
             -- block until receive the work
             work <- expect :: Process Work
             case work of
               -- if got a work, send the processed result
               Work i -> return ()
               -- otherwise, there must be no more work
               -- shutdown mapper
               Term   -> return ()
