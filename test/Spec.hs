import Test.Tasty
import Test.Tasty.HUnit (testCase, testCaseSteps)
import Test.Tasty.QuickCheck
import Test.HUnit

import IntMapProps

import Control.Monad.Trans.UnionFindDelete.Class
import Control.Monad.Trans.UnionFindDelete.UnionFindT
import qualified Control.Monad.Trans.UnionFindDelete.Internal.UnionFindSimple as Simple

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Default (def)
import qualified Data.Set as Set

sampleGraph :: [(Integer, Integer)]
sampleGraph =
  [ (2, 3)
  , (2, 5)
  , (2, 7)
  , (3, 5)
  , (5, 11)

  , (13, 17)
  , (17, 19)
  ]

connectedComponents :: [[Integer]]
connectedComponents =
  [ [2, 3, 5, 7, 11]
  , [13, 17, 19]
  ]

initializingUnionFind :: (Alternative m, MonadIO m, UnionFindMonad m Integer (Maybe Integer)) => m ()
initializingUnionFind = do
  val0 <- find 0
  val1 <- find 1
  liftIO $ val0 @?= def
  liftIO $ val1 @?= def

graphUnionFind :: (Alternative m, MonadIO m, UnionFindMonad m Integer (Set.Set Integer)) => Bool -> (String -> m ()) -> m ()
graphUnionFind testDelete step = do
  step "initializing the graph"
  forM_ sampleGraph $ uncurry union

  step "testing union find worked"
  forM_ connectedComponents $ \cc -> forM_ cc $ \c -> do
    valc <- find c
    liftIO $ valc @=? Set.fromList cc

  -- forget 3, then test that 2 still has the same value
  when testDelete $ do
    step "testing deleting works"
    forget 3
    val2 <- find 2
    liftIO $ val2 @=? Set.fromList [2, 3, 5, 7, 11]
    val3 <- find 3
    liftIO $ val3 @=? defFor (3 :: Integer)


prop_SpanningTreeInt :: [(Integer, Integer)] -> Bool
prop_SpanningTreeInt [] = True
prop_SpanningTreeInt tree = evalUnionFind ufProg def == Set.fromList (tree >>= \(a, b) -> [a, b])
  where
    ufProg = do
      forM_ tree $ uncurry union
      find $ fst $ head tree


main :: IO ()
main = defaultMain $ testGroup "union find test"
  [ testCase "(Simple) trivial union find" $ do
      Simple.evalUnionFindT initializingUnionFind def
  , testCaseSteps "(Simple) union find on sample graph works" $ \step -> do
      Simple.evalUnionFindT (graphUnionFind False $ liftIO . step) def
  , testCase "trivial union find" $ do
      evalUnionFindT initializingUnionFind def
  , testCaseSteps "union find on sample graph works" $ \step -> do
      evalUnionFindT (graphUnionFind True $ liftIO . step) def
  , testProperty "union find (without delete) on spanning tree" $
      forAll (spanningTree 0) prop_SpanningTreeInt
  ]
