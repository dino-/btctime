-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module BTCTime.BTCTime
   ( getLatestBlock
   , getBlockTime
   )
where

--import qualified BTCTime.Explorers.BitcoinchainCom as BitcoinchainCom
import qualified BTCTime.Explorers.BlockchainInfo as BlockchainInfo
import qualified BTCTime.Explorers.BlockexplorerCom as BlockexplorerCom
import qualified BTCTime.Explorers.BtcCom as BtcCom
import BTCTime.Types ( BlockexplorerException (..), BTCBlock )

import Control.Exception.Safe ( SomeException, throw, tryAny )
import Control.Monad ( unless )
import Data.Either ( partitionEithers )
import Data.List ( all )
import Data.Text ( Text )
import Data.Time ( UTCTime )


getLatestBlock :: IO (Either SomeException BTCBlock)
getLatestBlock = tryAny $ do
   eBlocks <- sequence
      [ BlockchainInfo.getLatestBlock
      , BtcCom.getLatestBlock
      , BlockexplorerCom.getLatestBlock
      --, BitcoinchainCom.getLatestBlock
      ]

   let (failures, successes) = partitionEithers eBlocks

   unless (null failures) $ do
      let results = unlines $ "Not all block explorers succeeded:" : (map show eBlocks)
      throw $ BlockexplorerException results

   unless (allTheSame successes) $ do
      let results = unlines $ "All successful results are not equal:" : (map show successes)
      throw $ BlockexplorerException results

   return . head $ successes


getBlockTime :: Text -> IO (Either SomeException UTCTime)
getBlockTime hash = tryAny $ do
   eTimes <- sequence $ map ($ hash)
      [ BlockchainInfo.getBlockTime
      , BtcCom.getBlockTime
      , BlockexplorerCom.getBlockTime
      --, BitcoinchainCom.getBlockTime
      ]

   let (failures, successes) = partitionEithers eTimes

   unless (null failures) $ do
      let results = unlines $ "Not all block explorers succeeded:" : (map show eTimes)
      throw $ BlockexplorerException results

   unless (allTheSame successes) $ do
      let results = unlines $ "All successful results are not equal:" : (map show successes)
      throw $ BlockexplorerException results

   return . head $ successes


allTheSame :: (Eq e) => [e] -> Bool
allTheSame [ ] = False
allTheSame [_] = True
allTheSame xs  = all (== head xs) (tail xs)
