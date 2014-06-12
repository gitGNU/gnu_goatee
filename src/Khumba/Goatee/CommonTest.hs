-- This file is part of Goatee.
--
-- Copyright 2014 Bryan Gardiner
--
-- Goatee is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Goatee is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with Goatee.  If not, see <http://www.gnu.org/licenses/>.

module Khumba.Goatee.CommonTest (tests) where

import qualified Control.Monad.State as State
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Writer (execWriter, tell)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Monoid (mappend, mempty)
import Khumba.Goatee.Common
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?), assertFailure)

{-# ANN module "HLint: ignore Use camelCase" #-}

tests = testGroup "Khumba.Goatee.Common" [
  listDeleteIndexTests,
  listReplaceTests,
  onLeftTests,
  onRightTests,
  andEithersTests,
  forTests,
  mapTupleTests,
  whenMaybeTests,
  condTests,
  if'Tests,
  andMTests,
  forIndexM_Tests,
  whileMTests,
  whileM'Tests,
  doWhileMTests,
  seqTests
  ]

listDeleteIndexTests = testGroup "listDeleteIndex" [
  testCase "deletes nothing from an empty list" $ do
    [] @=? listDeleteIndex (-1) ([] :: [()])
    [] @=? listDeleteIndex 0 ([] :: [()])
    [] @=? listDeleteIndex 1 ([] :: [()]),

  testCase "deletes the only item from a singleton list" $
    [] @=? listDeleteIndex 0 [()],

  testCase "deletes the head of a list" $
    [False] @=? listDeleteIndex 0 [True, False],

  testCase "deletes the tail of a list" $
    [Just 0, Nothing] @=? listDeleteIndex 2 [Just 0, Nothing, Just 1],

  testCase "deletes an inner item from a list" $
    [Just 0, Just 1] @=? listDeleteIndex 1 [Just 0, Nothing, Just 1],

  testCase "ignores a negative index" $ do
    [1, 2, 3] @=? listDeleteIndex (-1) [1, 2, 3]
    [1, 2, 3] @=? listDeleteIndex (-2) [1, 2, 3],

  testCase "ignores an index too large" $ do
    [1, 2, 3] @=? listDeleteIndex 3 [1, 2, 3]
    [1, 2, 3] @=? listDeleteIndex 4 [1, 2, 3]
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

forTests = testGroup "for" [
  testCase "passes an empty list through" $
    ([] @=?) =<< sequence (for [] $ const $ assertFailure "Nope."),

  testCase "operates on each element of a list" $
    [2, 2, 4, 6, 10] @=? for [1, 1, 2, 3, 5] (* 2)
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

if'Tests = testGroup "if'" [
  testCase "detects true" $ "yes" @=? if' "yes" "no" True,

  testCase "detects false" $ "no" @=? if' "yes" "no" False
  ]

andMTests = testGroup "andM" [
  testCase "empty list returns true" $ True @=? runIdentity (andM []),

  testCase "aborts after an immediate true" $ do
    ref <- newIORef 0
    result <- andM [addToRef ref 1 >> return False,
                    addToRef ref 2 >> return True]
    False @=? result
    (1 @=?) =<< readIORef ref,

  testCase "aborts on a false after trues" $ do
    ref <- newIORef 0
    result <- andM [addToRef ref 1 >> return True,
                    addToRef ref 2 >> return True,
                    addToRef ref 4 >> return False,
                    addToRef ref 8 >> return False,
                    addToRef ref 16 >> return False]
    False @=? result
    (7 @=?) =<< readIORef ref,

  testCase "executes all trues" $ do
    ref <- newIORef 0
    result <- andM [addToRef ref 1 >> return True,
                    addToRef ref 2 >> return True,
                    addToRef ref 4 >> return True,
                    addToRef ref 8 >> return True]
    True @=? result
    (15 @=?) =<< readIORef ref
  ]
  where addToRef ref num = modifyIORef ref (+ num)

forIndexM_Tests = testGroup "forIndexM_" [
  testCase "does nothing with an empty list" $
    "" @=? execWriter (forIndexM_ [] $ \_ _ -> tell "x"),

  testCase "iterates over all elements in order" $
    "(0,'a')(1,'b')(2,'c')" @=?
    execWriter (forIndexM_ "abc" $ \index value -> tell $ show (index, value))
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

whileM'Tests = testGroup "whileM'" [
  testCase "never executes the body if the first test returns Nothing" $
    "" @=? execWriter (whileM' (return Nothing) (const $ tell "x")),

  testCase "executes repeatedly as expected" $
    let test = get >>= \n -> return $ if n > 0 then Just n else Nothing
        body n = do tell $ show n
                    put (n - 1)
    in "321" @=? execWriter (runStateT (whileM' test body) 3)
  ]

doWhileMTests = testGroup "doWhileM" [
  testCase "executes a constantly-Left body once" $ do
    countVar <- newIORef 0
    result <- doWhileM () (const $ do modifyIORef countVar (+ 1)
                                      return $ Left "done")
    count <- readIORef countVar
    (1, "done") @=? (count, result),

  testCase "executes until a Left is returned" $
    execWriter (doWhileM 9 $ \n -> do
                   tell $ show n
                   return $ if n == 0 then Left () else Right $ n - 1)
    @=? "9876543210"
  ]

seqTests = testGroup "Seq" [
  testCase "mempty does nothing" $
    let Seq action = mempty
    in "" @=? execWriter action,

  testCase "mappend works" $
    let Seq action = Seq (tell "a") `mappend` Seq (tell "b")
    in "ab" @=? execWriter action
  ]
