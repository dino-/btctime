-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module BTCTime.Types
   ( BlockexplorerException (..)
   , BTCBlock (..)
   )
where

import Control.Exception ( Exception )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Type.Reflection ( Typeable )


newtype BlockexplorerException = BlockexplorerException String
  deriving (Show, Typeable)

instance Exception BlockexplorerException


data BTCBlock = BTCBlock
  { blHash :: Text
  , blTime :: UTCTime
  , blExplorer :: Text
  }
  deriving Show

instance Eq BTCBlock where
  x == y = (blHash x == blHash y) && (blTime x == blTime y)
