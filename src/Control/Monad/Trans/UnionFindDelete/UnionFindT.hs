{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Control.Monad.Trans.UnionFindDelete.UnionFindT
( UFIState
, newUFIState
, UnionFindT (..)
, runUnionFindT
, execUnionFindT
, evalUnionFindT
, runUnionFind
, execUnionFind
, evalUnionFind
, UFI.UFILinkInfo
) where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.UnionFindDelete.Class as UFC

import qualified Data.UnionFindDelete.AGRTZ as UFI

import Control.Applicative (Alternative)
import Control.Lens hiding (children)
import Control.Monad.IO.Class (MonadIO)
import Data.Default.Class (Default(def))
import Data.Kind (Type)
import qualified Control.Monad.State.Strict as S
import qualified Data.Map as Map

data UFIState a b = UFIState
  { _ufisEnv :: !(UFI.UnionFindAGRTZ b)
  , _ufisPreEnv :: !(Map.Map a UFI.NodeIdx)
  }

-- | Create a new empty state
newUFIState :: UFIState a b
newUFIState = UFIState UFI.empty Map.empty

instance Default (UFIState a b) where
  def = newUFIState

$(makeLenses ''UFIState)

newtype UnionFindT a b m r = UnionFindT { getUnionFind :: S.StateT (UFIState a b) m r }
  deriving (Functor, Applicative, Monad, Alternative, MonadIO, Trans.MonadTrans)

type UnionFind a b r = UnionFindT a b Identity r

runUnionFindT :: UnionFindT a b m r -> UFIState a b -> m (r, UFIState a b)
runUnionFindT = S.runStateT . getUnionFind

runUnionFind :: UnionFind a b r -> UFIState a b -> (r, UFIState a b)
runUnionFind uf = runIdentity . runUnionFindT uf

execUnionFindT :: Monad m => UnionFindT a b m r -> UFIState a b -> m (UFIState a b)
execUnionFindT = S.execStateT . getUnionFind

execUnionFind :: UnionFind a b r -> UFIState a b -> UFIState a b
execUnionFind uf = runIdentity . execUnionFindT uf

evalUnionFindT :: Monad m => UnionFindT a b m r -> UFIState a b -> m r
evalUnionFindT = S.evalStateT . getUnionFind

evalUnionFind :: UnionFind a b r -> UFIState a b -> r
evalUnionFind uf = runIdentity . evalUnionFindT uf

instance Monad m => S.MonadState (UFIState a b) (UnionFindT a b m) where
  state = UnionFindT . S.state

stateful :: Monad m => UnionFind a b r -> UnionFindT a b m r
stateful = S.state . S.runState . getUnionFind

findOrInitializeUFI :: (Ord a, UFC.DefaultFor a b) => a -> UnionFind a b UFI.NodeIdx
findOrInitializeUFI key = do
  envKey <- use (ufisPreEnv . at key)
  case envKey of
    Just eKey -> return eKey
    Nothing -> do
      eKey <- UnionFindT $ zoom ufisEnv $ UFI.create (UFC.defFor key)
      ufisPreEnv . at key ?= eKey
      return eKey

newtype UnionFindIntAccess (m :: Type -> Type) a b
  = UnionFindIntAccess
  { _ufiaEKey :: Int -- ^ the internal node, to which the data this access comes from points to
  }

instance UFC.DefaultFor a b => UFC.UnionFindAccessor (UnionFindIntAccess m a b) (UnionFindT a b m) where
  getLookup access = ufisEnv . UFI.find (_ufiaEKey access)

instance (Ord a, UFC.DefaultFor a b, Monad m) => UFC.UnionFind (UnionFindT a b m) where
  type UnionFindKey (UnionFindT a b m) = a
  type UnionFindState (UnionFindT a b m) = UFIState a b
  type UnionFindId (UnionFindT a b m) = UFI.UFILinkInfo b
  type UnionFindAccess (UnionFindT a b m) = UnionFindIntAccess m a b
  type UnionFindVal (UnionFindT a b m) = b

  findFor key = stateful $ UnionFindIntAccess <$> findOrInitializeUFI key
  unionWith combine key1 key2 = stateful $ do
    eKey1 <- findOrInitializeUFI key1
    eKey2 <- findOrInitializeUFI key2
    UnionFindT $ zoom ufisEnv $ UFI.union combine eKey1 eKey2
  forget key = stateful $ do
    existingKey <- ufisPreEnv . at key <.= Nothing
    case existingKey of
      Nothing -> pure ()
      Just eKey -> UnionFindT $ zoom ufisEnv $ UFI.delete eKey
