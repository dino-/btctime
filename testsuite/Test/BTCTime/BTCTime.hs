module Test.BTCTime.BTCTime
   ( test_blockTime )
   where

import Control.Exception ( SomeException, try )
import Data.Either ( fromRight, isRight )
import Data.Time ( UTCTime )
import Test.Hspec ( SpecWith, it, runIO, shouldBe, shouldSatisfy )

import BTCTime.BTCTime ( getBlockTime, getLatestBlock )
import BTCTime.Types ( BTCBlock (blHash, blTime) )


test_blockTime :: SpecWith ()
test_blockTime = do
   eLatestBlock <- runIO $ ((try getLatestBlock) :: IO (Either SomeException BTCBlock))
   it "acquired latest block" $ eLatestBlock `shouldSatisfy` isRight

   let latestBlock = fromRight undefined eLatestBlock
   eBlockTime <- runIO $ ((try $ getBlockTime (blHash latestBlock)) :: IO (Either SomeException UTCTime))
   it "acquired block time for block hash" $ eBlockTime `shouldSatisfy` isRight

   let blockTime = fromRight undefined eBlockTime
   it "timestamp should always be the same for a block hash" $
      blTime latestBlock `shouldBe` blockTime
