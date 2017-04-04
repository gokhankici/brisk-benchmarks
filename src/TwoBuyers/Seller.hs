{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module TwoBuyers.Seller (seller) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import TwoBuyers.Utils

seller :: () -> Process ()
seller _ = do
  self <- getSelfPid
  (Title buyer1 buyer2 _) <- expect
  send buyer1 (Quote self 100)
  send buyer2 (Quote self 100)

  msg <- expect

  case msg of
    OK pid -> do (OfferDetails _pid _str) <- expect
                 send pid (ShippingDate 1 1 1990)
    Quit _ -> return ()
      
