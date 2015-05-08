module Control.Bitcoin.Api.TransactionSpec where

import           Control.Concurrent                           (forkIO)
import           Control.Concurrent.MVar                      (newEmptyMVar,
                                                               putMVar,
                                                               takeMVar)
import           Control.Lens                                 ((^.))
import           Data.Conduit
import qualified Data.Conduit.List                            as CL

import qualified Network.Bitcoin.Api.Mining                   as Mining
import qualified Network.Bitcoin.Api.Transaction              as Transaction
import           Network.Bitcoin.Api.Types.UnspentTransaction (amount)
import qualified Network.Bitcoin.Api.Wallet                   as Wallet

import           Control.Bitcoin.Api.TestUtil                 (testClient)
import           Test.Hspec

spec :: Spec
spec = do
  describe "when testing transaction functions" $ do
    it "can watch for new transactions" $ do
      testClient $ \client -> do

        result <- newEmptyMVar

        -- This forks a thread, consumes a single transaction from the conduit,
        -- and puts it in the MVar.
        _ <- forkIO $ do
          list <- runConduit $ Transaction.watch client (Just 1) $= CL.take 1
          putMVar result list

        -- Now, let's generate a transaction and some blocks to put the
        -- the transaction in.
        utxs             <- Wallet.listUnspent client

        (length utxs) `shouldSatisfy` (>= 1)

        -- Calculate the total BTC of all unspent transactions
        let btc          = foldr (+) 0 $ map (^. amount) utxs

        addr             <- Wallet.newAddress client
        tx               <- Transaction.create client utxs [(addr, (btc - 0.0001))]
        (tx', completed) <- Transaction.sign client tx (Just utxs) Nothing

        completed `shouldBe` True

        _                <- Transaction.send client tx'
        _                <- Mining.generate client 10

        val <- takeMVar result

        length (val) `shouldBe` 1
