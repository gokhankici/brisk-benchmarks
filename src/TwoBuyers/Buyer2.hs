{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module TwoBuyers.Buyer2 (buyer2) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import TwoBuyers.Utils

buyer2 :: () -> Process ()
buyer2 _ = do
  self <- getSelfPid
  (Quote sellerPid _offer) <- expect
  (BuyerQuote _buyer1Pid _share) <- expect

  b <- liftIO $ getChar >>= \ c -> if c == 'a' then return True else return False

  if b
    then do send sellerPid (OK self)
            send sellerPid (OfferDetails self "bar")
            (SD _ _ _) <- expect
            return ()
    else send sellerPid (Quit self)

  return ()
