module Test.BTCTime.BTCTime
   ( test_blockTime )
   where

import Control.Exception ( fromException )
import Test.Hspec

import BTCTime.BTCTime ( getBlockTime, getLatestBlock )
import BTCTime.Types ( BlockexplorerException (..), BTCBlock (blHash, blTime) )


test_blockTime :: SpecWith ()
test_blockTime = do
  -- Get the latest block
  -- The type here will be: Either SomeException BTCBlock
  eLatestBlock <- runIO getLatestBlock

  -- Just for info purposes, print results of that call
  runIO $ case eLatestBlock of
    Left someException -> do
      putStrLn "\nFailed"
      maybe (print someException) (\(BlockexplorerException msg) -> putStr msg) $
        fromException someException
    Right latestBlock -> putStrLn $ "\nSucceeded\n" ++ (show latestBlock)

  -- If that's Right, use it to get the time for that hash
  -- The type here will be: Either SomeException UTCTime
  eBlockTime <- runIO $ either (return . Left) (getBlockTime . blHash) eLatestBlock

  -- Just for info purposes, print results of that call
  runIO $ case eBlockTime of
    Left someException -> do
      putStrLn "\nFailed"
      maybe (print someException) (\(BlockexplorerException msg) -> putStr msg) $
        fromException someException
    Right latestBlock -> putStrLn $ "\nSucceeded\n" ++ (show latestBlock)

  -- Now, confirm that, between the two separate http calls, the timestamp for
  -- that BTC block remained the same
  it "timestamp should always be the same for a block hash" $
    case (eLatestBlock, eBlockTime) of
      (Right lb, Right bt) -> bt `shouldBe` blTime lb
      (Right _ , Left  e ) -> expectationFailure . show $ e
      (Left  e , _       ) -> expectationFailure . show $ e
