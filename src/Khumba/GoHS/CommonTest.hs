module Khumba.GoHS.CommonTest (tests) where

import qualified Control.Monad.State as State
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Writer (execWriter, tell)
import Data.Monoid (mempty, mappend)
import Khumba.GoHS.Common
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

tests = testGroup "Khumba.GoHS.Common" [
  listReplaceTests,
  onLeftTests,
  onRightTests,
  andEithersTests,
  mapTupleTests,
  whenMaybeTests,
  condTests,
  whileMTests,
  seqTests
  ]

listReplaceTests = testGroup "listReplace" [
  testCase "accepts an empty list" $
    [] @=? listReplace False True [],

  testCase "replaces the first element" $
    [100, 2, 3] @=? listReplace 1 100 [1, 2, 3],

  testCase "replaces the last element" $
    [1, 2, 300] @=? listReplace 3 300 [1, 2, 3],

  testCase "replaces an interior element" $
    [1, 200, 3] @=? listReplace 2 200 [1, 2, 3],

  testCase "replaces multiple elements" $ do
    [1, 0, 3, 4, 0, 0, 3] @=? listReplace 2 0 [1, 2, 3, 4, 2, 2, 3]
    replicate 10 True @=? listReplace False True (replicate 10 False)
  ]

onLeftTests = testGroup "onLeft" [
  testCase "changes a Left" $
    Left 10 @=? onLeft (* 2) (Left 5 :: Either Int ()),

  testCase "doesn't change a Right" $
    Right False @=? onLeft (* 2) (Right False)
  ]

onRightTests = testGroup "onRight" [
  testCase "doesn't change a Left" $
    Left False @=? onRight (* 2) (Left False),

  testCase "changes a Right" $
    Right 10 @=? onRight (* 2) (Right 5 :: Either () Int)
  ]

andEithersTests = testGroup "andEithers" [
  testCase "returns Right for an empty list" $
    Right [] @=? andEithers ([] :: [Either () ()]),

  testCase "returns Right when given all Rights" $ do
    Right [1] @=? andEithers [Right 1 :: Either () Int]
    Right [1, 2] @=? andEithers [Right 1, Right 2 :: Either () Int]
    Right [1, 2, 3] @=? andEithers [Right 1, Right 2, Right 3 :: Either () Int],

  testCase "returns Left when there is a single Left" $ do
    Left [1] @=? andEithers [Left 1 :: Either Int ()]
    Left [1] @=? andEithers [Left 1, Right 2]
    Left [2] @=? andEithers [Right 1, Left 2]
    Left [2] @=? andEithers [Right 1, Left 2, Right 3]
    Left [3] @=? andEithers [Right 1, Right 2, Left 3],

  testCase "returns Left when there are many Lefts" $ do
    Left [1, 2] @=? andEithers [Left 1, Left 2, Right 3]
    Left [1, 3] @=? andEithers [Left 1, Right 2, Left 3]
    Left [2, 3] @=? andEithers [Right 1, Left 2, Left 3]
    Left [1, 2, 3] @=? andEithers [Left 1, Left 2, Left 3 :: Either Int ()]
  ]

mapTupleTests = testGroup "mapTuple" [
  testCase "updates both values" $
    (9, 16) @=? mapTuple (^ 2) (3, 4)
  ]

whenMaybeTests = testGroup "whenMaybeTests" [
  testCase "doesn't invoke its function when given Nothing" $
    0 @=? State.execState (whenMaybe Nothing $ \x -> State.put (x * 2)) 0,

  testCase "invokes its function when given a Just" $
    6 @=? State.execState (whenMaybe (Just 3) $ \x -> State.put (x * 2)) 0
  ]

condTests = testGroup "cond" [
  testCase "accepts an empty list of cases" $
    True @=? cond True [],

  testCase "returns the default when all cases are false" $ do
    0 @=? cond 0 [(False, 1)]
    0 @=? cond 0 [(False, 1), (False, 2)]
    0 @=? cond 0 [(False, 1), (False, 2), (False, 3)],

  testCase "returns a single true case" $ do
    3 @=? cond 0 [(False, 1), (False, 2), (True, 3)]
    2 @=? cond 0 [(False, 1), (True, 2), (False, 3)]
    1 @=? cond 0 [(True, 1), (False, 2), (False, 3)],

  testCase "returns the first true case" $ do
    3 @=? cond 0 [(False, 1), (False, 2), (True, 3)]
    2 @=? cond 0 [(False, 1), (True, 2), (True, 3)]
    1 @=? cond 0 [(True, 1), (True, 2), (True, 3)]
  ]

whileMTests = testGroup "whileM" [
  testCase "never executes the body if the first test returns false" $
    "" @=? execWriter (whileM (return False) (tell "x")),

  testCase "executes repeatedly as expected" $
    let test = do n <- get
                  tell $ show n
                  if n > 0
                    then put (n - 1) >> return True
                    else return False
        body = tell "x"
    in "3x2x1x0" @=? execWriter (runStateT (whileM test body) 3)
  ]

seqTests = testGroup "Seq" [
  testCase "mempty does nothing" $
    let Seq action = mempty
    in "" @=? execWriter action,

  testCase "mappend works" $
    let Seq action = Seq (tell "a") `mappend` Seq (tell "b")
    in "ab" @=? execWriter action
  ]
