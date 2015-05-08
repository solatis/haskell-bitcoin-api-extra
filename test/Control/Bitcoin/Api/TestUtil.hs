module Control.Bitcoin.Api.TestUtil (testClient) where

import qualified Data.Text                  as T (pack)

import           Network.Bitcoin.Api.Client

testClient :: (Client -> IO a) -> IO a
testClient = withClient "127.0.0.1" 18332 (T.pack "user") (T.pack "pass")
