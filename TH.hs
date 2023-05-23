{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import GHC.Generics
import Data.Coerce (coerce)
import Language.Haskell.TH

-- a simple type class for demonstration purposes
class A a where
  a :: a

instance               A Int    where a = 97
instance               A Char   where a = 'a'
instance  A a       => A [a]    where a = [a]
instance (A a, A b) => A (a, b) where a = (a, a)

-- how to automatically get A instances for arbitrary record types?
--   (i.e. datatypes with one constructor)

-- approach 1: GHC generics

-- the representation of a type seems to always be a type constructor * -> *
-- which ignores its argument, so we pass Dummy as the argument
data Dummy
-- the class of generic representations for which we can do the deriving
class Helper (rep :: * -> *) where helper :: rep Dummy

-- K1 (i.e. Rep0) is a newtype wrapping fields of the constructor
instance A a => Helper (K1 r a) where
  helper = (coerce :: a -> K1 r a Dummy) a
  -- i.e.: helper = K1 a

-- M1 is a newtype supplying metadata about the representation
instance Helper rep => Helper (M1 i c rep) where
  helper = (coerce :: rep Dummy -> M1 i c rep Dummy) helper
  -- i.e.: helper = M1 helper

-- :*: is the product of functors
instance (Helper rep1, Helper rep2) => Helper (rep1 :*: rep2) where
  helper = helper :*: helper

-- if `a` implements `Generic` such that its representation only uses K1, M1, and products,
-- then we can automatically derive an implementation of the `a` method.
-- this lets the user generate `GenericA` instances using `DeriveAnyClass`.
class (Generic a, Helper (Rep a)) => GenericA a where
  genericA :: a
  genericA = to helper

-- Lets the user get an A instance from by deriving GenericA.
-- (we could also have used a constrained default impl in A, like:
--   default a :: (Generic a, Helper (Rep a)) => a
--   a = to helper
-- )
-- The overlapping instance is acceptable here, because
-- we should only have GenericA instances for new ADTs.
instance {-# OVERLAPS #-} GenericA a => A a where a = genericA

-- approach 2: Template Haskell

deriveA :: Name -> Q [Dec]
deriveA ty = do
  info <- reify ty
  dec <- case info of
    TyConI dec -> pure dec
    _ -> fail $ concat ["Type ", show ty, " is not an ADT."]
  constructor <- do
    (ctx, _, ty_params, kind, constructors, _) <-
      case dec of
        DataD ctx name ty_params kind constructors derivings -> pure (ctx, name, ty_params, kind, constructors, derivings)
        NewtypeD ctx name ty_params kind constructor derivings -> pure (ctx, name, ty_params, kind, [constructor], derivings)
        _ -> fail $ concat ["Type ", show ty, " is not `newtype` or `data`."]
    case ctx of [] -> pure ()
                _ -> fail "We don't yet support having any datatype contexts"
    case ty_params of [] -> pure ()
                      _ -> fail "We don't yet support having any type parameters"
    case kind of Nothing -> pure ()
                 _ -> fail "We don't yet support kind annotations"
    case constructors of
      [constructor] -> pure constructor
      _ -> fail "We can only derive for product types, not sum types"
  (conName, paramTypes) <- case constructor of
    NormalC       name fields -> pure (name, [ty | (_, ty) <- fields]) -- ignore strictness/packedness metadata
    RecC          name fields -> pure (name, [ty | (_, _, ty) <- fields]) -- for records we also have field names to ignore
    InfixC field1 name field2 -> pure (name, [ty | (_, ty) <- [field1, field2]])
    _                         -> fail "We don't support any fancy constructors"
  [d|
      instance A $(pure (ConT ty)) where
        a = $(foldl (\f param -> [| $f (a :: $(pure param)) |])
                    (pure (ConE conName))
                    paramTypes)
    |]
