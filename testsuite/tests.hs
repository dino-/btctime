-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Test.Hspec ( hspec )

import Test.BTCTime.BTCTime ( test_blockTime )


main :: IO ()
main = hspec $ do
   test_blockTime
