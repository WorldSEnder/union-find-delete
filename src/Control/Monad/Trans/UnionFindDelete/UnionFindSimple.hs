-- | A union-only-find implementation without a delete operation. Mostly as a reference implementation
-- and for estimating the performance overhead of maintaining the link structure.
module Control.Monad.Trans.UnionFindDelete.UnionFindSimple
( UFIState
, newUFIState
, UnionFindT(..)
, runUnionFindT
, execUnionFindT
, evalUnionFindT
, UFILinkInfo(UFILinkInfo)
, ufiliRoot
, ufiliRank
) where

import Control.Monad.Trans.UnionFindDelete.Class
import Control.Monad.Trans.UnionFindDelete.Util

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Default.Class

import qualified Control.Monad.State.Strict as S
import           Data.Function (on)

import qualified Data.IntMap as IMap
import qualified Data.Map as Map

-- UFI = Union-Find-Int
data UFILink b
  = Info {-# UNPACK #-} !Int !b
  -- ^ rank + value
  | Link {-# UNPACK #-} !Int
  -- ^ index of parent

-- use strict Maybe here?
type InternalLinkMap b = IMap.IntMap (UFILink b)

data UFIState a b
  = UFIState
  { _ufisEnv :: !(InternalLinkMap b) 
  , _ufisPreEnv :: !(Map.Map a Int)
  , _ufisNextId :: {-# UNPACK #-} !Int
  }

$(makeLenses ''UFIState)

data UFILinkInfo b
  = UFILinkInfo
  { _ufiliRoot :: {-# UNPACK #-} !Int
  , _ufiliRank :: {-# UNPACK #-} !Int
  }

$(makeLenses ''UFILinkInfo)

instance Eq (UFILinkInfo b) where
  (==) = (==) `on` _ufiliRoot

newtype UnionFindT a b m r = UnionFindT { getUnionFind :: S.StateT (UFIState a b) m r }
  deriving (Functor, Applicative, Monad, Alternative, MonadIO)

runUnionFindT :: UnionFindT a b m r -> UFIState a b -> m (r, UFIState a b)
runUnionFindT = S.runStateT . getUnionFind

execUnionFindT :: Monad m => UnionFindT a b m r -> UFIState a b -> m (UFIState a b)
execUnionFindT = S.execStateT . getUnionFind

evalUnionFindT :: Monad m => UnionFindT a b m r -> UFIState a b -> m r
evalUnionFindT = S.evalStateT . getUnionFind

instance Monad m => S.MonadState (UFIState a b) (UnionFindT a b m) where
  state = UnionFindT . S.state

findInUIFMap :: forall b. b -> Int -> IndexedLens' (UFILinkInfo b) (InternalLinkMap b) b
findInUIFMap defVal key p env = go p key where
  f b = indexed @(UFILinkInfo b) p b

  insertLinkInfo key rank env nextB = IMap.insert key (Info rank nextB) env

  updLink key nextKey env = IMap.insert key (Link newRoot) env where
    newRoot =
      case IMap.lookup nextKey env of
        Just (Info _ _) -> nextKey
        Just (Link root) -> root
        Nothing -> error "link shouldn't go away"

  go p key = case IMap.lookup key env of
    Just (Info rank val) -> insertLinkInfo key rank env <$> f (UFILinkInfo key rank) val
    Just (Link nextKey) -> updLink key nextKey <$> go p nextKey
    Nothing -> insertLinkInfo key 0 env <$> f (UFILinkInfo key 0) defVal

data UnionFindIntAccess a b (m :: * -> *)
  = UnionFindIntAccess
  { _ufiaDef :: !b
  , _ufiaEKey :: Int -- ^ the internal node, to which the data this access comes from points to
  }

instance UnionFindAccessor (UnionFindIntAccess a b m) (UnionFindT a b m) where
  getLookup access = ufisEnv . findInUIFMap (_ufiaDef access) (_ufiaEKey access)

instance (DefaultFor a b, Ord a, Monad m) => UnionFind (UnionFindT a b m) where
  type UnionFindKey (UnionFindT a b m) = a
  type UnionFindState (UnionFindT a b m) = UFIState a b
  type UnionFindId (UnionFindT a b m) = UFILinkInfo b
  type UnionFindVal (UnionFindT a b m) = b
  type UnionFindAccess (UnionFindT a b m) = UnionFindIntAccess a b m

  findFor key = do
    let defVal = defFor key
    envKey <- use (ufisPreEnv . at key)
    eKey <- case envKey of
      Just eKey -> return eKey
      Nothing -> do
        eKey <- ufisNextId <%= (+1)
        ufisPreEnv . at key ?= eKey
        return eKey
    -- do a single find operation
    _ <- iharvest (ufisEnv . findInUIFMap defVal eKey)
    return $ UnionFindIntAccess defVal eKey

  unionWith combine key1 key2 = do
    (li1, data1) <- find' key1
    (li2, data2) <- find' key2
    let root1 = li1 ^. ufiliRoot
        root2 = li2 ^. ufiliRoot
        combined = combine data1 data2
    if li1 == li2 then return () else do
      case compare (li1 ^. ufiliRank) (li2 ^. ufiliRank) of
        LT -> do
          ufisEnv . at root1 ?= Link root2
          ufisEnv . at root2 ?= Info (li2 ^. ufiliRank) combined
        EQ -> do
          ufisEnv . at root1 ?= Link root2
          ufisEnv . at root2 ?= Info (li2 ^. ufiliRank + 1) combined
        GT -> do
          ufisEnv . at root1 ?= Info (li1 ^. ufiliRank) combined
          ufisEnv . at root2 ?= Link root1

  forget key = error "deleting not supported"

newUFIState :: UFIState a b
newUFIState = UFIState IMap.empty Map.empty 0

instance Default (UFIState a b) where
  def = newUFIState
