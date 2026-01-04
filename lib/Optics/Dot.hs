{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Optics.Dot
  ( DotOptics (..),
    HasDotOptic (..),
    GenericDotOptics (..),
    GenericDotOpticsMethod,
    FieldDotOptics (..),
    FieldDotOpticsMethod,
    the,
    --
    SetField (..),
  )
where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Optics.Core

instance
  ( DotOptics u,
    HasDotOptic (DotOpticsMethod u) name u v a b,
    JoinKinds k A_Lens m,
    AppendIndices is NoIx ks
  ) =>
  HasField name (Optic k is s t u v) (Optic m ks s t a b)
  where
  getField o = o % (dotOptic @(DotOpticsMethod u) @name @u @v @a @b)

-- | Helper typeclass, used only to specify the method for deriving dot optics.
class DotOptics s where
  type DotOpticsMethod s :: Type

-- | Produce an optic according to the given method.
--
-- The @name v -> u a b w@ fundep could be added but doesn't seem to be necessary.
-- Could it improve type inference?
type HasDotOptic :: Type -> Symbol -> Type -> Type -> Type -> Type -> Constraint
class HasDotOptic method name u v a b | name u -> a b where
  dotOptic :: Lens u v a b

data GenericDotOpticsMethod

-- | Used for deriving 'DotOptics' using DerivingVia. The wrapped type is not used for anything.
newtype GenericDotOptics s = GenericDotOptics s

instance DotOptics (GenericDotOptics s) where
  type DotOpticsMethod (GenericDotOptics s) = GenericDotOpticsMethod

-- | Produce an optic using the optics' package own generic machinery.
instance
  (GField name s t a b) =>
  HasDotOptic GenericDotOpticsMethod name s t a b
  where
  dotOptic = gfield @name

data FieldDotOpticsMethod

-- | Used for deriving 'DotOptics' using DerivingVia. The wrapped type is not used for anything.
newtype FieldDotOptics s = FieldDotOptics s

instance DotOptics (FieldDotOptics s) where
  type DotOpticsMethod (FieldDotOptics s) = FieldDotOpticsMethod

-- | Produce an optic using the 'HasField'/'SetField' machinery form "GHC.Records".
instance
  ( HasField name s a,
    SetField name s a,
    s ~ t,
    a ~ b
  ) =>
  -- if you change to @name s s a a@, a compilation error crops up in tests.
  HasDotOptic FieldDotOpticsMethod name s t a b
  where
  dotOptic = Optics.Core.lens (getField @name) (flip (setField @name))

-- | Identity 'Iso'. Used as a starting point for dot access.
the :: Iso s t s t
the = Optics.Core.equality

-- | This should be in base in the future.
type SetField :: forall {k}. k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  setField :: a -> r -> r
