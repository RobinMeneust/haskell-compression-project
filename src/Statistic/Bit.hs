{- |
  Module : Statistic.Bit
  Description : A module representing a bit
  Maintainer : Nino Hamel, Mathis tempo
-}
module Statistic.Bit(Bit(Zero, One)) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen


instance Arbitrary Bit where
  arbitrary = elements [Zero, One]

-- | A bit is either a zero-valued or one-valued
data Bit = Zero | One
  deriving (Bounded, Enum, Eq, Ord)

instance Show Bit where
  show Zero = "0"
  show One  = "1"

