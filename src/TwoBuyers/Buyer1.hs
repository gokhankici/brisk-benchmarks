{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module TwoBuyers.Buyer1 (buyer1) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import TwoBuyers.Utils

buyer1 :: (ProcessId, ProcessId) -> Process ()
buyer1 (sellerPid, buyer2Pid) = do
  self <- getSelfPid
  send sellerPid (Title self buyer2Pid "foo")
  (Quote _ n) <- expect

  send buyer2Pid (BuyerQuote self (n `div` 2))

  return ()
