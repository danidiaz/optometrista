{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Optics.Dot
  ( HasField (..),
    RecordDotOptics (..),
    GenericDotOptics (..),
    the,
  )
where

import Data.Coerce
import Data.Kind
import GHC.Records
import GHC.TypeLits
import Optics.Core

instance
  ( u ~ ß,
    RecordDotOptics name u v a b ß,
    JoinKinds k A_Lens m,
    AppendIndices is NoIx ks
  ) =>
  HasField name (Optic k is s t u v) (Optic m ks s t a b)
  where
  getField o = o % (runDotOptic (dotOptic @name @u @v @a @b @ß))

newtype DotOptic name s t a b ß = DotOptic {_dotOptic :: Lens s t a b}

runDotOptic :: DotOptic name s t a b ß -> Lens s t a b
runDotOptic DotOptic {_dotOptic} = _dotOptic

class RecordDotOptics name s t a b ß | name s -> t a b, name t -> s a b where
  dotOptic :: DotOptic name s t a b ß

type GenericDotOptics :: Type -> Type
newtype GenericDotOptics s = GenericDotOptics s

instance (GField name s t a b) => RecordDotOptics name s t a b (GenericDotOptics s) where
  dotOptic = DotOptic (gfield @name)

the :: Iso a b a b
the = Optics.Core.equality

-- | This should be in base in the future.
type SetField :: forall {k}. k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  setField :: a -> r -> r
