{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Optics.Coerce
import GHC.Generics
import Optics.Core
import Optics.Lens.Dot

data Whole a = Whole
  { whole1 :: Int,
    part :: Part a
  }
  deriving stock (Generic)

instance GField name (Whole p) (Whole q) a b => RecordDotOptics name (Whole p) (Whole q) a b where
    theOptic = gfield @name

data Part a = Part
  { part1 :: Bool,
    subpart :: Subpart a
  }
  deriving stock (Generic)

instance GField name (Part p) (Part q) a b => RecordDotOptics name (Part p) (Part q) a b where
    theOptic = gfield @name

data Subpart a = Subpart
  { subpart1 :: String,
    foo :: a
  }
  deriving stock (Generic)

instance GField name (Subpart p) (Subpart q) a b => RecordDotOptics name (Subpart p) (Subpart q) a b where
    theOptic = gfield @name

whole :: Whole Int
whole = Whole 0 (Part True (Subpart "wee" 7))

whole' :: Whole Bool
whole' = whole & the.part.subpart.foo .~ False

main :: IO ()
main = putStrLn "Test suite not yet implemented."
