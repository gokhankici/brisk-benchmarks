{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Auction.Seller (seller) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import Auction.Utils

seller :: () -> Process ()
seller _ = do
  self <- getSelfPid
  (Buyers buyers) <- expect
  forM buyers (\_ -> do bi <- expect
                        case bi of
                          Notify p -> do send p (Price self 0)
                                         (Order _) <- expect
                                         return ()
                          Stop _ -> return ())
  return ()
      
