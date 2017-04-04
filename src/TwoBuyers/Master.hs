{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module TwoBuyers.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import TwoBuyers.Utils
import TwoBuyers.Buyer1
import TwoBuyers.Buyer2
import TwoBuyers.Seller

remotable [ 'buyer1, 'buyer2, 'seller ]

master :: NodeId -> Process ()
master node = do
  _self <- getSelfPid
  sellerPid <- spawn node $ $(mkBriskClosure 'seller) ()
  buyer2Pid <- spawn node $ $(mkBriskClosure 'buyer2) ()
  buyer1Pid <- spawn node $ $(mkBriskClosure 'buyer1) (sellerPid, buyer2Pid)
  return ()
