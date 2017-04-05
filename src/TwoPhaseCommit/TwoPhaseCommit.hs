{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module TwoPhase (main) where

import Control.Monad (forM, foldM)
import Control.Distributed.Process
import Control.Distributed.BriskStatic
import Control.Distributed.Process.Closure
import Control.Distributed.Process.SymmetricProcess
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

import GHC.Base.Brisk
import System.Directory

data AcceptorResponse = Accept ProcessId | Reject
                      deriving (Typeable, Generic)
instance Binary AcceptorResponse

data AcceptorAck = ACK
                      deriving (Typeable, Generic)
instance Binary AcceptorAck

data CoordMessage = Commit ProcessId | Rollback ProcessId
                      deriving (Typeable, Generic)
instance Binary CoordMessage

coordPid (Commit p)   = p      
coordPid (Rollback p) = p

forEach :: SymSet i -> (i -> Process a) -> Process ()
forEach xs body
  = foldM (\_ i -> body i >> return ()) () xs

acceptor :: Process ()
acceptor = do
  me            <- getSelfPid
  (who, fn)     <- expect :: Process (ProcessId, String)

  -- Do the transaction
  exists <- liftIO $ doesDirectoryExist fn
  let msg = if exists then Accept me else Reject
  send who msg

  -- Wait for message to commit
  msg <- expect :: Process CoordMessage

  send (coordPid msg) ACK

coord :: String -> Int -> SymSet ProcessId -> Process ()
coord f n as = do foldM query () as
                  n_ <- foldM countVotes 0 as
                  if n == n_ then
                    forEach as $ doCommit ()
                  else
                    forEach as $ doAbort ()
                  forEach as $ \_ -> expect :: Process AcceptorAck
  where
    doCommit () x  = do { me <- getSelfPid; send x (Commit me)   }
    doAbort  () x  = do { me <- getSelfPid; send x (Rollback me) }
    query () x     = do { me <- getSelfPid; send x (me, f)       }
    countVotes x y = do msg <- expect
                        case msg of
                          Accept _ -> return (x + 1)
                          Reject   -> return x


remotable ['acceptor]

main :: String -> [NodeId] -> Process ()
main fn nodes = do me <- getSelfPid
                   as <- spawnSymmetric nodes $ $(mkBriskStaticClosure 'acceptor)
                   let n = length nodes
                   coord fn n as
