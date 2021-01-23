module IntMapProps where

import Test.QuickCheck

-- | Generate a random spanning tree
spanningTree :: Integral a => a -> Gen [(a, a)]
spanningTree base = reverse <$> sized go
  where
    go 0 = return []
    go n = do
      cs <- go (n - 1)
      prec <- chooseInt (0, n - 1)
      return $ (base + fromIntegral prec, base + fromIntegral n) : cs
