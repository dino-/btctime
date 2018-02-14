-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module BTCTime.Explorers.BlockchainInfo
   ( getBlockTime, getLatestBlock )
where

import BTCTime.Types ( BlockexplorerException (..), BTCBlock (..) )

import Control.Exception.Safe ( SomeException, throw, tryAny )
import Control.Lens ( (^.) )
import Data.Aeson ( FromJSON, (.:), decode, parseJSON, withObject )
import Data.String.Conv ( toS )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Network.Wreq ( get, responseBody )


newtype BlockchainInfo = BlockchainInfo { unwrap :: BTCBlock }

instance FromJSON BlockchainInfo where
  parseJSON = withObject "BTCBlock" $ \o -> do
    block <- BTCBlock
      <$> (o .: "hash")
      <*> ((\epoch -> posixSecondsToUTCTime . realToFrac
          $ (epoch :: Int)) <$> (o .: "time"))
      <*> (return "blockchain.info")
    return $ BlockchainInfo block


getLatestBlock :: IO (Either SomeException BTCBlock)
getLatestBlock = tryAny $ do
  -- Retrieve the JSON document for the latest block (may fail)
  response <- get "https://blockchain.info/latestblock"

  -- Decode the JSON in the response body and retrieve the block hash
  -- and timestamp (may fail)
  let body = response ^. responseBody
  maybe (throw $ BlockexplorerException (toS body)) (return . unwrap) $ decode body


getBlockTime :: Text -> IO (Either SomeException UTCTime)
getBlockTime hash = tryAny $ do
  -- Retrieve the JSON document for a specific block (may fail)
  response <- get $ concat ["https://blockchain.info/rawblock/", (toS hash)]

  -- Decode the JSON in the response body and retrieve the block hash
  -- and timestamp (may fail)
  let body = response ^. responseBody
  maybe (throw $ BlockexplorerException (toS body)) (return . blTime . unwrap) $ decode body
