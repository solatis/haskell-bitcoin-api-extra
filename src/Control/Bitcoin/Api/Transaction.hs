{-# LANGUAGE OverloadedStrings #-}

-- | This module provides functionality to manipulate raw transaction. It
--   automatically interprets transactions using the `bitcoin-tx` package, so
--   you can work with actual 'Btc.Transaction' objects rather than their
--   serialized format.

module Control.Bitcoin.Api.Transaction where

import           Data.Maybe                                   (fromMaybe)

import qualified Data.Conduit                                 as C (Source)

import           Control.Concurrent                           (forkIO,
                                                               killThread,
                                                               myThreadId,
                                                               threadDelay)
import           Control.Concurrent.STM.TBMQueue              (isClosedTBMQueue,
                                                               newTBMQueue,
                                                               writeTBMQueue)
import           Control.Monad                                (unless)
import           Control.Monad.IO.Class                       (liftIO)
import           Control.Monad.STM                            (atomically)
import           Data.Conduit.TQueue                          (sourceTBMQueue)

import qualified Data.Bitcoin.Transaction                     as Btc
import qualified Data.Bitcoin.Block                           as Btc

import qualified Network.Bitcoin.Api.Blockchain               as Blockchain
import qualified Network.Bitcoin.Api.Types                    as T

import           Network.Bitcoin.Api.Types.UnspentTransaction hiding (confirmations)

-- | Watches incoming transactions and yields new transactions as soon as they
--   are are inside a block. Note that this launches a background thread which
--   stops as soon as the Conduit is closed.
watch :: T.Client                    -- ^ Our client session context
      -> Maybe Integer               -- ^ Minimum amount of confirmations. Should be 1 or higher. A default value of 6 is used.
      -> C.Source IO Btc.Transaction -- ^ Conduit that generates transactions
watch client Nothing              = watch client (Just 6)
watch client (Just confirmations) = do
  chan      <- liftIO $ atomically $ newTBMQueue 16
  curHeight <- liftIO blockHeight
  _         <- liftIO $ forkIO $ watchNext chan curHeight

  sourceTBMQueue chan

  where

    -- | Calculates the height of the block we currently are looking for
    blockHeight = do
      limit <- Blockchain.getBlockCount client
      return (limit - confirmations)

    -- | Watches the current height of the block chain, and continues only
    --   when the height changes.
    watchNext chan height = do
      cur <- blockHeight

      if cur > height
        then go chan (height + 1)
        else threadDelay 1000000 >> watchNext chan height

    -- | Fills the chan with all transactions from the block at `height`,
    --   and then continues waiting until a next block is available.
    go chan height = do
      block <- Blockchain.getBlock client =<< Blockchain.getBlockHash client height
      tid   <- myThreadId


      result <- mapM (insert chan) (Btc.blockTxns block)
      let isClosed = False `elem` result

      if isClosed
        then killThread tid
        else watchNext chan height

    -- | Inserts a transaction into the queue. Blocks if the queue is full.
    --   Returns True if write succeeded, False is the queue was closed.
    insert chan tx = atomically $ do
      isClosed <- isClosedTBMQueue chan
      unless isClosed (writeTBMQueue chan tx)
      return isClosed
