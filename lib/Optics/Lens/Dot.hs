{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Optics.Lens.Dot where

import GHC.Records
import Optics.Core
import Optics.Coerce
import GHC.TypeLits
import Data.Proxy
import Data.Kind
    
instance 
    (RecordDotOptics name u v a b,
     JoinKinds k A_Lens m, 
     AppendIndices is NoIx ks)
    =>
    HasField name (Optic k is s t u v) (Optic m ks s t a b) where
        getField o = o % theOptic @name

the :: Iso a b a b
the = Optics.Core.equality

class RecordDotOptics name u v a b | name u -> v a b, name v -> u a b where
    theOptic :: Lens u v a b

-- newtype GenericOptics r = GenericOptics r

-- https://stackoverflow.com/questions/53009549/haskell-derivingvia-on-multi-param-type-classes-with-fun-deps

-- instance GField name u v a b => RecordDotOptics name (GenericOptics u) v a b where
--     theOptic _ = Optics.Coerce.coerceS (gfield @name)

-- instance 
--     GField (name :: Symbol) a a b b
--     =>
--     HasField name (Optic A_Lens NoIx s s a a) (Optic A_Lens NoIx s s b b) where
--         getField theLens = theLens %% gfield @name 
--     

-- instance 
--     GField (name :: Symbol) u v a b
--     =>
--     HasField name (Optic A_Lens NoIx s t u v) (Optic A_Lens NoIx s t a b) where
--         getField = (% gfield @name)

type SetField :: forall {k} . k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  setField :: a -> r -> r