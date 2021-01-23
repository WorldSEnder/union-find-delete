{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Monad.Trans.UnionFindDelete.Class
( UnionFind(..)
, UnionFindAccessor(..)
, type UnionFind'
, type UnionFindMonad
, find
, find'
, insert
, union
, DefaultFor(..)
, DefaultConst(..)
) where

import Control.Monad.Trans.UnionFindDelete.Util

import Control.Lens
import qualified Control.Monad.State.Class as S
import Data.Default.Class

-- | An accessor is a proxy for finding and modifying a value associated with a specific key.
class UnionFindAccessor acc m | acc -> m where
  -- | Get a lens into the union find state focusing on the value associated with the key with which
  -- this accessor was created.
  getLookup :: acc -> IndexedLens' (UnionFindId m) (UnionFindState m) (UnionFindVal m)

-- | Class of union find monads, where one can find 'UnionFindVal m' associated with 'UnionFindKey m's.
class (S.MonadState (UnionFindState m) m, Eq (UnionFindId m), UnionFindAccessor (UnionFindAccess m) m) => UnionFind m where
  type UnionFindKey m
  type UnionFindVal m

  type UnionFindState m
  -- | The type used for identity, which is provided as an additional index when looking up a value
  -- in the state. For example, in the standard union find forest implementation, this is the set representative node.
  type UnionFindId m
  type UnionFindAccess m
  -- | Get access to the associated value for the given key.
  -- 
  -- Note that finding a value will effect the underlying storage even if no value is associated, yet.
  -- Consider
  -- 
  -- > do
  -- >   union key1 key2
  -- >   insert key1 value
  --
  -- One would expect that looking up the element for 'key2' finds 'value'.
  findFor :: UnionFindKey m -> m (UnionFindAccess m)
  -- | Union the value of two keys. If only one of the values has been set,
  -- prefer that one.
  unionWith :: (UnionFindVal m -> UnionFindVal m -> UnionFindVal m) -> UnionFindKey m -> UnionFindKey m -> m ()
  -- | Forget a value again. Note that this operation might invalidate
  -- the prism gotten from 'find', which may then 'error' or lead to leaked memory
  -- that can not be recovered.
  forget :: UnionFindKey m -> m ()

type UnionFind' m a b = (UnionFind m, a ~ UnionFindKey m, b ~ UnionFindVal m)
type UnionFindMonad m a b = (Monad m, UnionFind' m a b)

-- | Find the value and an equality comparable identity attached to the value. The identity compares equal for
-- two keys iff they are associated to the same value.
find' :: (UnionFindMonad m a b) => a -> m (UnionFindId m, UnionFindVal m)
find' key = findFor key >>= iharvest . getLookup

-- | Find the value associated to a key.
find :: (UnionFindMonad m a b) => a -> m b
find key = findFor key >>= harvest . getLookup

-- | Assign the value associated to a key.
insert :: (UnionFindMonad m a b) => a -> b -> m ()
insert key val = do
  place <- findFor key
  getLookup place .= val

-- | 'union = unionWith (<>)', for the case where the value is a 'Monoid'.
union :: (UnionFindMonad m a b, Monoid b) => a -> a -> m ()
union = unionWith (<>)

-- | Used to initialize the value of a key that is not yet associated with a value, in some Union-Find implementations.
class DefaultFor a b where
  -- | A specific morphism for producing values 'b' from innputs 'a'.
  defFor :: a -> b

-- | Newtype for producing instances of 'DefaultFor' for values with a 'Default' class, in case
-- the value doesn't depend on the key.
newtype DefaultConst b = DefaultConst { getDefaultConst :: b }
  deriving (Eq, Semigroup, Monoid, Show)

instance Default b => Default (DefaultConst b) where
  def = DefaultConst def

instance Default b => DefaultFor a (DefaultConst b) where
  defFor = const $ DefaultConst def
