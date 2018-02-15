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

import Control.Exception ( throwIO )
import Data.List ( all )
import Data.Text ( Text )
import Data.Time ( UTCTime )


getLatestBlock :: IO BTCBlock
getLatestBlock = do
   blocks <- sequence
      [ BlockchainInfo.getLatestBlock
      , BtcCom.getLatestBlock
      , BlockexplorerCom.getLatestBlock
      --, BitcoinchainCom.getLatestBlock
      ]

   if allTheSame blocks
      then return . head $ blocks
      else throwIO $ BlockexplorerException $ unlines
            $ "All results are not equal:" : (map show blocks)


getBlockTime :: Text -> IO UTCTime
getBlockTime hash = do
   times <- sequence $ map ($ hash)
      [ BlockchainInfo.getBlockTime
      , BtcCom.getBlockTime
      , BlockexplorerCom.getBlockTime
      --, BitcoinchainCom.getBlockTime
      ]

   if allTheSame times
      then return . head $ times
      else throwIO $ BlockexplorerException $ unlines
            $ "All results are not equal:" : (map show times)


allTheSame :: (Eq e) => [e] -> Bool
allTheSame [ ] = False
allTheSame [_] = True
allTheSame xs  = all (== head xs) (tail xs)
