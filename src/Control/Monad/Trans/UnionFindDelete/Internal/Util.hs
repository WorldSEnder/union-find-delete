module Control.Monad.Trans.UnionFindDelete.Internal.Util
( harvest
, iharvest
, harvests
) where

import Control.Lens
import qualified Control.Monad.State.Class as S
import qualified Data.Profunctor.Strong as PS

harvest :: (S.MonadState s m) => Over (->) ((,) a) s s a a -> m a
harvest l = l <<%= id
{-# INLINE harvest #-}

iharvest :: (S.MonadState s m) => Over (Indexed i) ((,) (i, a)) s s a a -> m (i, a)
iharvest l = l %%@= \i a -> ((i, a), a)
{-# INLINE iharvest #-}

harvests :: (S.MonadState s m) => Over (->) ((,) b) s s a a -> (a -> b) -> m b
harvests l f = l %%= lmap (\a -> (a, a)) (PS.first' f)
{-# INLINE harvests #-}
