-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module BTCTime.Explorers.BtcCom
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


newtype BtcCom = BtcCom { unwrap :: BTCBlock }

instance FromJSON BtcCom where
  parseJSON = withObject "BTCBlock" $ \o -> do
    datao <- o .: "data"
    block <- BTCBlock
      <$> (datao .: "hash")
      <*> ((\epoch -> posixSecondsToUTCTime . realToFrac
          $ (epoch :: Int)) <$> (datao .: "timestamp"))
      <*> (return "btc.com")
    return $ BtcCom block


getLatestBlock :: IO (Either SomeException BTCBlock)
getLatestBlock = tryAny $ do
  -- Retrieve the JSON document for the latest block (may fail)
  response <- get "https://chain.api.btc.com/v3/block/latest"

  -- Decode the JSON in the response body and retrieve the block hash
  -- and timestamp (may fail)
  let body = response ^. responseBody
  maybe (throw $ BlockexplorerException (toS body)) (return . unwrap) $ decode body


getBlockTime :: Text -> IO (Either SomeException UTCTime)
getBlockTime hash = tryAny $ do
  -- Retrieve the JSON document for a specific block (may fail)
  response <- get $ concat ["https://chain.api.btc.com/v3/block/", (toS hash)]

  -- Decode the JSON in the response body and retrieve the block hash
  -- and timestamp (may fail)
  let body = response ^. responseBody
  maybe (throw $ BlockexplorerException (toS body)) (return . blTime . unwrap) $ decode body
