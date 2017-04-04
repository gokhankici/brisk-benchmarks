{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Auction.Buyer (buyer) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import Auction.Utils

buyer :: () -> Process ()
buyer _ = do
  self <- getSelfPid
  (Sellers sellers) <- expect
  forM sellers (\_ -> do si <- expect
                         case si of
                           Match s -> do send s (Notify self)
                                         (Price _ _) <- expect
                                         send s (Order self)
                           Quit s -> send s (Quit self))
  return ()
  
