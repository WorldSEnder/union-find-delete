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
, UFILinkInfo
) where

import qualified Control.Monad.Trans.UnionFindDelete.Class as UFC
import qualified Control.Monad.Trans.UnionFindDelete.Util as UFC

import Control.Applicative (Alternative)
import Control.Exception (assert)
import Control.Lens hiding (children)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Default.Class (Default(def))
import Data.Function (on)
import Data.Kind (Type)
import Data.Maybe (isNothing)
import qualified Control.Monad.State.Strict as S
import qualified Data.IntMap as IMap
import qualified Data.Map as Map

import GHC.Stack (HasCallStack)

type NodeIdx = Int

unsafeIx :: NodeIdx -> Lens' (IMap.IntMap b) b
unsafeIx key = at key . assertJust where
  assertJust f (Just x) = Just <$> f x
  assertJust _ Nothing = error $ "internal invariant: expected " ++ show key ++ " to exist"

assertM :: Applicative m => Bool -> m ()
assertM = flip assert $ pure ()

-- UFI = Union-Find-Int with deletion

-- refer to https://link.springer.com/chapter/10.1007/11523468_7#citeas
-- available with slight modifications at
-- https://www.cs.princeton.edu/courses/archive/fall05/cos528/handouts/Union-Find%20with%20Constant%20Time%20Deletions.pdf

-- there's quite a lot more book keeping this time around.
-- We maintain, for each node:
--  - a list of children
--  - a list of children that are not leafs
-- We do so, by linking the children together in a doubly-linked, index based fashion.

-- Nodes that have been deleted are marked as vacant, before they might get removed.
-- We maintain the invariant that a vacant node has at least two children. This implies
-- that leaf nodes can not be vacant, and each tree is at least half full.
data UFIRoot b
  = UFIRoot_
  { _ufirRank :: {-# UNPACK #-} !Int
  , _ufirData :: !b
  }

$(makeLenses ''UFIRoot)

data UFILink
  = UFILink_
  { _ufilParent :: {-# UNPACK #-} !NodeIdx -- ^ the parent name
  , _ufilLeftSib :: {-# UNPACK #-} !NodeIdx -- ^ the left sibling
  , _ufilRightSib :: {-# UNPACK #-} !NodeIdx -- ^ the right sibling
  , _ufilLeftInnerSib :: {-# UNPACK #-} !NodeIdx -- ^ the left sibling with children
  , _ufilRightInnerSib :: {-# UNPACK #-} !NodeIdx -- ^ the left sibling with children
  }

$(makeLenses ''UFILink)

data UFIFlavor b
  = UFIFlavorRoot !(UFIRoot b)
  | UFIFlavorLink !UFILink

$(makePrisms ''UFIFlavor)

pattern UFIRoot :: Int -> b -> UFIFlavor b
pattern UFIRoot r d = UFIFlavorRoot (UFIRoot_ r d)

pattern UFILink :: NodeIdx -> NodeIdx -> NodeIdx -> NodeIdx -> NodeIdx -> UFIFlavor b
pattern UFILink {_ufifParent,_ufifLeftSib,_ufifRightSib,_ufifLeftInnerSib,_ufifRightInnerSib}
  = UFIFlavorLink (UFILink_ _ufifParent _ufifLeftSib _ufifRightSib _ufifLeftInnerSib _ufifRightInnerSib)

{-# COMPLETE UFIRoot, UFILink #-}

data UFINode b
  = UFINode
  { _ufinVacant :: !Bool
  , _ufinLeftmostChild :: {-# UNPACK #-} !NodeIdx
  , _ufinLeftmostInnerChild :: {-# UNPACK #-} !NodeIdx
  , _ufinFlavor :: UFIFlavor b
  }

$(makeLenses ''UFINode)

_AssertRoot :: HasCallStack => Lens' (UFINode b) (UFIRoot b)
_AssertRoot = ufinFlavor . inner where
  inner :: HasCallStack => Lens' (UFIFlavor b) (UFIRoot b)
  inner _ (UFIFlavorLink _) = error "node is not a root"
  inner f (UFIFlavorRoot r) = UFIFlavorRoot <$> f r

_Root :: Traversal' (UFINode b) (UFIRoot b)
_Root = ufinFlavor . _UFIFlavorRoot

_AssertLink :: HasCallStack => Lens' (UFINode b) UFILink
_AssertLink = ufinFlavor . inner where
  inner :: HasCallStack => Lens' (UFIFlavor b) UFILink
  inner f (UFIFlavorLink l) = UFIFlavorLink <$> f l
  inner _ (UFIFlavorRoot _) = error "node is not a link"

_Link :: Traversal' (UFINode b) UFILink
_Link = ufinFlavor . _UFIFlavorLink

-- use strict Maybe here?
type InternalLinkMap b = IMap.IntMap (UFINode b)

noLink :: NodeIdx
noLink = -1

newNode :: b -> UFINode b
newNode val = UFINode False noLink noLink $ UFIRoot 0 val

instance UFC.DefaultFor a b => UFC.DefaultFor a (UFINode b) where
  defFor = newNode . UFC.defFor

data UFIState a b = UFIState
  { _ufisEnv :: !(InternalLinkMap b)
  , _ufisPreEnv :: !(Map.Map a NodeIdx)
  , _ufisNextId :: {-# UNPACK #-} !NodeIdx
  }

-- | Create a new empty state
newUFIState :: UFIState a b
newUFIState = UFIState IMap.empty Map.empty 0

instance Default (UFIState a b) where
  def = newUFIState

$(makeLenses ''UFIState)

-- | Node identity
data UFILinkInfo b = UFILinkInfo
  { _ufiliRoot :: {-# UNPACK #-} !NodeIdx
  , _ufiliRank :: {-# UNPACK #-} !Int
  }

$(makeLenses ''UFILinkInfo)

instance Eq (UFILinkInfo b) where
  (==) = (==) `on` _ufiliRoot

newtype UnionFindT a b m r = UnionFindT { getUnionFind :: S.StateT (UFIState a b) m r }
  deriving (Functor, Applicative, Monad, Alternative, MonadIO)

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

-- Remove a node from the parent's inner link list
removeInnerNode :: NodeIdx -> S.State (InternalLinkMap b) ()
removeInnerNode nodeIdx = do
  node <- use (unsafeIx nodeIdx . ufinFlavor)
  case node of
    -- if it's not a node, nothing to do here
    UFIFlavorRoot{} -> pure ()
    UFIFlavorLink nodeLink -> do
      let lsib = nodeLink ^. ufilLeftInnerSib
          rsib = nodeLink ^. ufilRightInnerSib
          parent = nodeLink ^. ufilParent

      when (lsib /= noLink) $ unsafeIx lsib . _AssertLink . ufilRightInnerSib .= rsib
      when (rsib /= noLink) $ unsafeIx rsib . _AssertLink . ufilLeftInnerSib  .= lsib
      unsafeIx parent . ufinLeftmostInnerChild %= \c -> if c == nodeIdx then rsib else c

-- Remove a node from its parent's links. Node must be a Link, not a Root.
-- Returns the old parent index
removeNode :: NodeIdx -> S.State (InternalLinkMap b) (NodeIdx, UFINode b)
removeNode nodeIdx = do
  nodeLink <- use (unsafeIx nodeIdx . _AssertLink)
  let lsib = nodeLink ^. ufilLeftSib
      rsib = nodeLink ^. ufilRightSib
      parentIdx = nodeLink ^. ufilParent

  removeInnerNode nodeIdx
  when (lsib /= noLink) $ unsafeIx lsib . _AssertLink . ufilRightSib .= rsib
  when (rsib /= noLink) $ unsafeIx rsib . _AssertLink . ufilLeftSib  .= lsib
  newParent <- unsafeIx parentIdx <%= (ufinLeftmostChild %~ \c -> if c == nodeIdx then rsib else c)
  -- also remove the parent from the grand parent's inner child list, if node was the last child
  when (newParent ^. ufinLeftmostChild == noLink) $ removeInnerNode parentIdx
  return (parentIdx, newParent)

-- Append a node to a new parent (setting its flavor to Link in the process)
appendNode :: NodeIdx -> NodeIdx -> S.State (InternalLinkMap b) ()
appendNode nodeIdx parentIdx = do
  let node, parent :: Lens' (InternalLinkMap b) (UFINode b)
      node = unsafeIx nodeIdx
      parent = unsafeIx parentIdx
  exNode <- use (unsafeIx nodeIdx)
  exParent <- use (unsafeIx parentIdx)
  let hasChildren = exNode ^. ufinLeftmostChild /= noLink
      firstChild = exParent ^. ufinLeftmostChild
      firstInnerChild = exParent ^. ufinLeftmostInnerChild
      newLink = UFILink
        { _ufifParent = parentIdx
        , _ufifRightSib = firstChild
        , _ufifRightInnerSib = if hasChildren then firstInnerChild else noLink
        , _ufifLeftSib = noLink
        , _ufifLeftInnerSib = noLink
        }
  -- parent must either be a link with children, or a root.
  assertM (firstChild /= noLink || has _Root exParent)

  parent . ufinLeftmostChild .= nodeIdx
  when hasChildren $ parent . ufinLeftmostInnerChild .= nodeIdx

  when (firstChild /= noLink) $
    unsafeIx firstChild . _AssertLink . ufilLeftSib .= nodeIdx
  when (hasChildren && firstInnerChild /= noLink) $
    unsafeIx firstInnerChild . _AssertLink . ufilLeftInnerSib .= nodeIdx
  node . ufinFlavor .= newLink

children :: Getter (InternalLinkMap b) (UFINode b -> [(NodeIdx, UFINode b)])
children = to conv
  where
  conv env = go . view ufinLeftmostChild
    where
    go i | i == noLink = []
    go i = let node = (env ^. unsafeIx i)
            in (i, node) : go (node ^. _AssertLink . ufilRightSib)

innerChildren :: Getter (InternalLinkMap b) (UFINode b -> [(NodeIdx, UFINode b)])
innerChildren = to conv
  where
  conv env = go . view ufinLeftmostInnerChild
    where
    go i | i == noLink = []
    go i = let node = (env ^. unsafeIx i)
            in (i, node) : go (node ^. _AssertLink . ufilRightInnerSib)

-- Shortcut the node to a new parent, higher up in the tree. Maintains invariants
shortcut :: NodeIdx -> NodeIdx -> S.State (InternalLinkMap b) ()
shortcut nodeIdx toIdx = do
  -- remove the node and get the (modified) parent node
  (parentIdx, parent) <- removeNode nodeIdx
  assertM (toIdx /= parentIdx)
  -- parent is surely a Link, since there is yet a higher node (toIdx)
  assertM (has _Link parent)
  -- if the parent is vacant and would be left with a single child
  -- shortcut the parent for that child and remove the parent
  when (parent ^. ufinVacant) $ use children ?? parent >>= \case
    [(singlChildIx, _)] -> do
      let gparentIdx = parent ^?! _Link . ufilParent
      -- First append the node to the grandparent, then remove parent
      -- This never leaves the grandparent without children, and avoids
      -- unnecessary modifications to the grand-grandparent's inner children list
      appendNode singlChildIx gparentIdx
      _ <- removeNode parentIdx
      -- finally, delete parent, since it's now a vacant leaf node
      at parentIdx .= Nothing
    _ -> pure ()
  -- finally, reattach the node to its new parent
  appendNode nodeIdx toIdx

-- vacate the node: set status to vacant, then restore invariants if the node is a link.
-- return the node index from which to start "doing some work" (see paper lemma 4)
vacate :: NodeIdx -> S.State (InternalLinkMap b) NodeIdx
vacate toVacIdx = do
  exNode <- unsafeIx toVacIdx <%= (ufinVacant .~ True)
  reestablishInvariant nodeEmptyCase toVacIdx exNode  
  where
  reestablishInvariant emptyCase nodeIdx node
    -- all is well if the node is not vacant
    | not (node ^. ufinVacant) = return nodeIdx
    | otherwise = case node ^. ufinFlavor of
      -- or if the node is a root node
      UFIFlavorRoot _ -> return nodeIdx
      -- if the node is a link, inspect how many children it has
      UFIFlavorLink nodeLink -> use children ?? node >>= \case
        -- if the node has no remaining children remove it entirely
        [] -> emptyCase nodeIdx nodeLink
        -- if the node has just a single child now, reestablish invariant
        -- by shortcutting the child to node's parent
        [(singlChildIx, _)] -> do
          let parentIdx = nodeLink ^. ufilParent
          (_nodeIdx, _) <- removeNode singlChildIx
          assertM (nodeIdx == _nodeIdx)
          appendNode singlChildIx parentIdx
          (_parentIdx, _) <- removeNode nodeIdx
          assertM (parentIdx == _parentIdx)
          -- actually delete the node in our mapping
          at nodeIdx .= Nothing
          return parentIdx
        -- more than one child --> invariant okay
        (_:_:_) -> return nodeIdx

  nodeEmptyCase nodeIdx nodeLink = do
    let parentIdx = nodeLink ^. ufilParent
    (_parentIdx, parent) <- removeNode nodeIdx
    assertM (parentIdx == _parentIdx)
    at nodeIdx .= Nothing
    -- since the invariant at the parent was still established before node has been removed
    -- we are guaranteed that the recursive call will not run into this case again
    reestablishInvariant (\_ _ -> assertM False >> return parentIdx) parentIdx parent

-- Do a bit of maintainance on the union find structure, basically by shortcutting a few nodes if possible
-- to establish the correct (amortized) runtime bounds. We do this after every deletion
lemma4 :: NodeIdx -> S.State (InternalLinkMap b) (Maybe (NodeIdx, Int))
lemma4 nodeIdx = do
  node <- use (unsafeIx nodeIdx)
  case node ^. ufinFlavor of
    UFIFlavorRoot _ -> return Nothing
    UFIFlavorLink link -> do
      use innerChildren ?? node >>= \case
        -- if no grandchildren, no work can be performed
        [] -> return $ Just (link ^. ufilParent, 0)
        (firstChildIdx, firstChild) : _
          | not (firstChild ^. ufinVacant) -> do
            -- if the first child is not vacant, we can safely shortcut its first child
            -- (which existance is guaranteed since its an inner child) to the node worked on.
            shortcut (firstChild ^. ufinLeftmostChild) nodeIdx
            return $ Just (nodeIdx, 10)
          | otherwise -> use children ?? firstChild >>= \case
            -- if there are three or more grand children, shortcutting any of them preserves invariants
            (firstGChildIdx, _) : _ : _ : _ -> do
              shortcut firstGChildIdx nodeIdx
              return $ Just (nodeIdx, 10)
            [fc, sc] -> handleTwoGrandChildren firstChildIdx fc sc
            -- firstChild has at least 2 children itself (by invariant)
            _ -> error "invariant: vacant node has less than 2 children"
  where
    handleVacantGrandchild gchildIdx gchild = do
      assertM (gchild ^. ufinVacant)
      -- since this grand child is vacant, it must have at least two children
      assertM (gchild ^. ufinLeftmostChild /= noLink)
      -- shortcut any of them to node
      shortcut (gchild ^. ufinLeftmostChild) nodeIdx
      -- if gchild had exactly two children, this shortcutting also removed gchild
      -- diminishing our returns by a bit
      -- TODO: does it actually make sense to check this? Seems like little is gained
      -- by accurately measuring this, at the cost of another indexing operation.
      gchildRemoved <- uses (at gchildIdx) isNothing
      return $ Just (nodeIdx, if gchildRemoved then 7 else 16)

    handleTwoGrandChildren firstChildIdx (firstGChildIdx, firstGChild) (secondGChildIdx, secondGChild)
      | firstGChild ^. ufinVacant = handleVacantGrandchild firstGChildIdx firstGChild
      | secondGChild ^. ufinVacant = handleVacantGrandchild secondGChildIdx secondGChild
      | otherwise = do
        -- if both are not vacant, enough work is done be shortcutting them both to node
        shortcut firstGChildIdx nodeIdx
        -- the other child is automatically shortcut to node, since its parent (firstChild) would
        -- end up as a vacant node with a single child, which gets correct by shortcutting that child
        -- assert that by observing that firstChild was removed
        use (at firstChildIdx) >>= assertM . isNothing
        return $ Just (nodeIdx, 15)

findInternal :: forall b. NodeIdx -> IndexedLens' (UFILinkInfo b) (InternalLinkMap b) b
findInternal key p env = case IMap.lookup key env of
  Nothing -> errBadKey
  Just n | n ^. ufinVacant -> errBadKey
         | otherwise -> snd (go key)
  where
    errBadKey = error "key error: value has been deleted"
    updInfo ckey cenv newB = cenv
      & unsafeIx ckey . _AssertRoot . ufirData .~ newB

    updLink ckey newRoot = S.execState (shortcut ckey newRoot)

    go ckey = case IMap.lookup ckey env of
      Just l -> case l ^. ufinFlavor of
        UFIFlavorRoot r
          -> let info = UFILinkInfo ckey (r ^. ufirRank)
              in (ckey, updInfo ckey env <$> indexed @(UFILinkInfo b) p info (r ^. ufirData))
        UFIFlavorLink lk
          -> let (root, env') = go (lk ^. ufilParent)
              in (root, updLink ckey root <$> env')
      Nothing -> error "internal invariant: key error"

findOrInitializeUFI :: (Ord a, UFC.DefaultFor a b) => a -> UnionFind a b NodeIdx
findOrInitializeUFI key = do
  envKey <- use (ufisPreEnv . at key)
  case envKey of
    Just eKey -> return eKey
    Nothing -> do
      eKey <- ufisNextId <%= (+ 1)
      ufisPreEnv . at key ?= eKey
      ufisEnv . at eKey ?= UFC.defFor key
      return eKey

unionInternal
  :: (b -> b -> b)
  -> NodeIdx -> NodeIdx -> S.State (InternalLinkMap b) ()
unionInternal combine key1 key2 = do
  (li1, data1) <- UFC.iharvest (findInternal key1)
  (li2, data2) <- UFC.iharvest (findInternal key2)
  let root1 = li1 ^. ufiliRoot
      root2 = li2 ^. ufiliRoot
      combined = combine data1 data2
  unless (li1 == li2) $ case compare (li1 ^. ufiliRank) (li2 ^. ufiliRank) of
    -- the root nodes are already roots, so we don't have to worry
    -- about overwriting parent's linking information
    LT -> do
      unsafeIx root2 . ufinFlavor .= UFIRoot (li2 ^. ufiliRank) combined
      appendNode root1 root2
    EQ -> do
      unsafeIx root2 . ufinFlavor .= UFIRoot (li2 ^. ufiliRank + 1) combined
      appendNode root1 root2
    GT -> do
      unsafeIx root1 . ufinFlavor .= UFIRoot (li1 ^. ufiliRank) combined
      appendNode root2 root1

deleteInternal :: NodeIdx -> S.State (InternalLinkMap b) ()
deleteInternal nodeIdx = vacate nodeIdx >>= doSomeWork 45
  where
    doSomeWork :: Int -> NodeIdx -> S.State (InternalLinkMap b) ()
    doSomeWork workLeft _ | workLeft <= 0 = return ()
    doSomeWork workLeft workIdx = do
      workRecord <- lemma4 workIdx
      case workRecord of
        Nothing -> return () -- hit a root node, stop
        Just (nextNode, workDone) -> doSomeWork (workLeft - workDone) nextNode

newtype UnionFindIntAccess (m :: Type -> Type) a b
  = UnionFindIntAccess
  { _ufiaEKey :: Int -- ^ the internal node, to which the data this access comes from points to
  }

instance UFC.DefaultFor a b => UFC.UnionFindAccessor (UnionFindIntAccess m a b) (UnionFindT a b m) where
  getLookup access = ufisEnv . findInternal (_ufiaEKey access)

instance (Ord a, UFC.DefaultFor a b, Monad m) => UFC.UnionFind (UnionFindT a b m) where
  type UnionFindKey (UnionFindT a b m) = a
  type UnionFindState (UnionFindT a b m) = UFIState a b
  type UnionFindId (UnionFindT a b m) = UFILinkInfo b
  type UnionFindAccess (UnionFindT a b m) = UnionFindIntAccess m a b
  type UnionFindVal (UnionFindT a b m) = b

  findFor key = stateful $ UnionFindIntAccess <$> findOrInitializeUFI key
  unionWith combine key1 key2 = stateful $ do
    eKey1 <- findOrInitializeUFI key1
    eKey2 <- findOrInitializeUFI key2
    UnionFindT $ zoom ufisEnv $ unionInternal combine eKey1 eKey2
  forget key = stateful $ do
    existingKey <- ufisPreEnv . at key <.= Nothing
    case existingKey of
      Nothing -> pure ()
      Just eKey -> UnionFindT $ zoom ufisEnv $ deleteInternal eKey
