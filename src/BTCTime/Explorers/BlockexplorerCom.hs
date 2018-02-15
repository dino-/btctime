-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module BTCTime.Explorers.BlockexplorerCom
   ( getBlockTime, getLatestBlock )
where

import BTCTime.Types ( BlockexplorerException (..), BTCBlock (..) )

import Control.Exception ( throwIO )
import Control.Lens ( (^.) )
import Data.Aeson ( FromJSON, (.:), decode, parseJSON, withObject )
import Data.String.Conv ( toS )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Network.Wreq ( get, responseBody )


newtype Hash = Hash Text

instance FromJSON Hash where
  parseJSON = withObject "Hash" $ \o -> do
    Hash <$> (o .: "lastblockhash")


newtype BlockchainInfo = BlockchainInfo { unwrap :: BTCBlock }

instance FromJSON BlockchainInfo where
  parseJSON = withObject "BTCBlock" $ \o -> do
    block <- BTCBlock
      <$> (o .: "hash")
      <*> ((\epoch -> posixSecondsToUTCTime . realToFrac
          $ (epoch :: Int)) <$> (o .: "time"))
      <*> (return "blockexplorer.com")
    return $ BlockchainInfo block


getLatestBlock :: IO BTCBlock
getLatestBlock = do
  -- First, retrieve the latest block hash (may fail)
  hashResponse <- get "https://blockexplorer.com/api/status?q=getLastBlockHash"
  let hashBody = hashResponse ^. responseBody
  (Hash hash) <- maybe (throwIO $ BlockexplorerException (toS hashBody)) return $ decode hashBody

  -- Retrieve the JSON document for the latest block (may fail)
  response <- get $ concat ["https://blockexplorer.com/api/block/", (toS hash)]

  -- Decode the JSON in the response body and retrieve the block hash
  -- and timestamp (may fail)
  let body = response ^. responseBody
  maybe (throwIO $ BlockexplorerException (toS body)) (return . unwrap) $ decode body


getBlockTime :: Text -> IO UTCTime
getBlockTime hash = do
  -- Retrieve the JSON document for a specific block (may fail)
  response <- get $ concat ["https://blockexplorer.com/api/block/", (toS hash)]

  -- Decode the JSON in the response body and retrieve the block hash
  -- and timestamp (may fail)
  let body = response ^. responseBody
  maybe (throwIO $ BlockexplorerException (toS body)) (return . blTime . unwrap) $ decode body
