import Test.Tasty
import Test.Tasty.HUnit (testCase, testCaseSteps)
import Test.HUnit

import Control.Monad.Trans.UnionFindDelete.Class
import Control.Monad.Trans.UnionFindDelete.UnionFindT
import qualified Control.Monad.Trans.UnionFindDelete.UnionFindSimple as Simple

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Default (def)
import Data.Monoid (Product(..))

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

initializingUnionFind :: (Alternative m, MonadIO m, UnionFindMonad m Integer (DefaultConst (Product Integer))) => m ()
initializingUnionFind = do
  val0 <- find 0
  val1 <- find 1
  liftIO $ val0 @?= def
  liftIO $ val1 @?= def

graphUnionFind :: (Alternative m, MonadIO m, UnionFindMonad m Integer (DefaultConst (Product Integer))) => Bool -> (String -> m ()) -> m ()
graphUnionFind testDelete step = do
  step "initializing the graph"
  forM_ sampleGraph $ \(start, end) -> do
    insert start $ DefaultConst $ Product start
    insert end $ DefaultConst $ Product end
  forM_ sampleGraph $ \(start, end) -> do
    unionWith (<>) start end

  step "testing union find worked"
  forM_ connectedComponents $ \cc -> forM_ cc $ \c -> do
    valc <- find c
    liftIO $ valc @=? mconcat (DefaultConst . Product <$> cc)

  -- forget 3, then test that 2 still has the same value
  when testDelete $ do
    step "testing deleting works"
    forget 3
    val2 <- find 2
    liftIO $ val2 @=? DefaultConst (Product $ 2 * 3 * 5 * 7 * 11)
    val3 <- find 3
    liftIO $ val3 @=? def

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
  ]
