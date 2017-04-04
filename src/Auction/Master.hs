{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Auction.Master (master) where

import GHC.Base.Brisk
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Control.Monad (forM)

import Auction.Utils
import Auction.Buyer
import Auction.Seller

remotable [ 'buyer, 'seller ]

master :: [NodeId] -> Process ()
master nodes = do
  _self <- getSelfPid
  sellers <- spawnSymmetric nodes $ $(mkBriskClosure 'seller) ()
  buyers  <- spawnSymmetric nodes $ $(mkBriskClosure 'buyer) ()
  forM sellers (\pid -> send pid (Buyers buyers))
  forM buyers (\pid -> do
                  send pid (Sellers sellers)
                  forM sellers (\s -> do
                                   msg <- liftIO $ getChar >>= \ c -> if c == 'a'
                                                                      then (Match s)
                                                                      else (Quit s)
                                   send pid msg)
              )
  return ()
