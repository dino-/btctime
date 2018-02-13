-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- WARNING: The blockchain explorer that this module communicates with has
   stale info as of 2017-02-13. This source code is present because it's
   helpful to develop and run tests against. DO NOT USE IT in production.
-}

module BTCTime.Explorers.BitcoinchainCom
   ( getBlockTime, getLatestBlock )
where

import BTCTime.Types

import Control.Exception.Safe ( SomeException, throw, tryAny )
import Control.Lens ( (^.) )
import Data.Aeson ( FromJSON, Value (Array, Object), (.:), decode, parseJSON )
import Data.Aeson.Types ( typeMismatch )
import Data.String.Conv ( toS )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Data.Vector ( head )
import Network.Wreq ( get, responseBody )
import Prelude hiding ( head, null )


newtype BitcoinchainCom = BitcoinchainCom { unwrap :: BTCBlock }

instance FromJSON BitcoinchainCom where
  parseJSON (Array a) = parseJSON . head $ a

  parseJSON (Object o) = do
    block <- BTCBlock
      <$> (o .: "hash")
      <*> ((\epoch -> posixSecondsToUTCTime . realToFrac
          $ (epoch :: Int)) <$> (o .: "time"))
      <*> (return "bitcoinchain.com")
    return $ BitcoinchainCom block

  parseJSON invalid = typeMismatch "BTCBlock" invalid


getLatestBlock :: IO (Either SomeException BTCBlock)
getLatestBlock = tryAny $ do
  -- Retrieve the JSON document for the latest block (may fail)
  response <- get "https://api-r.bitcoinchain.com/v1/blocks/1"

  -- Decode the JSON in the response body and retrieve the block hash
  -- and timestamp (may fail)
  let body = response ^. responseBody
  maybe (throw $ BlockexplorerException (toS body)) (return . unwrap) $ decode body


getBlockTime :: Text -> IO (Either SomeException UTCTime)
getBlockTime hash = tryAny $ do
  -- Retrieve the JSON document for a specific block (may fail)
  response <- get $ concat ["https://blockchain.info/blocks/", (toS hash)]

  -- Decode the JSON in the response body and retrieve the block hash
  -- and timestamp (may fail)
  let body = response ^. responseBody
  maybe (throw $ BlockexplorerException (toS body)) (return . blTime . unwrap) $ decode body
