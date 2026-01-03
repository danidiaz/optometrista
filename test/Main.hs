{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

-- https://stackoverflow.com/questions/53009549/haskell-derivingvia-on-multi-param-type-classes-with-fun-deps
module Main (main) where

import Optics.Coerce
import GHC.TypeLits
import GHC.Generics
import Optics.Core
import Optics.Lens.Dot
import GHC.Records

data Whole a = Whole
  { whole1 :: Int,
    part :: Part a
  }
  deriving stock (Generic, Show)

instance GField name (Whole p) (Whole q) a b => RecordDotOptics name (Whole p) (Whole q) a b where
    theOptic = gfield @name

data Part a = Part
  { part1 :: Bool,
    subpart :: Subpart a
  }
  deriving stock (Generic, Show)

instance GField name (Part p) (Part q) a b => RecordDotOptics name (Part p) (Part q) a b where
    theOptic = gfield @name

data Subpart a = Subpart
  { subpart1 :: String,
    foo :: a,
    subpart2 :: YetAnotherSubpart
  }
  deriving stock (Generic, Show)

instance GField name (Subpart p) (Subpart q) a b => RecordDotOptics name (Subpart p) (Subpart q) a b where
    theOptic = gfield @name

data YetAnotherSubpart = YetAnotherSubpart  {
        wee :: Int,
        bar :: Int
    }
    deriving Show

instance (HasField name YetAnotherSubpart x, SetField name YetAnotherSubpart x)  =>
    RecordDotOptics name (Subpart a) (Subpart a) x x where
    theOptic = Optics.Core.lens (getField @name) (flip (setField @name))

instance SetField "wee" YetAnotherSubpart Int where
    setField wee r = r { wee }

whole :: Whole Int
whole = Whole 0 (Part True (Subpart "wee" 7 (YetAnotherSubpart 2 3)) )

whole' :: Whole Bool
whole' = whole & the.part.subpart.foo .~ False

main :: IO ()
main = do 
    print whole
    print whole'
