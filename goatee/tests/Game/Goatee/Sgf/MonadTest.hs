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

module Game.Goatee.Sgf.MonadTest (tests) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (forM_, liftM, replicateM_, void)
import Control.Monad.Writer (Writer, execWriter, runWriter, tell)
import Data.List (unfoldr)
import Data.Maybe (fromJust, maybeToList)
import Game.Goatee.Sgf.Board
import Game.Goatee.Sgf.Monad
import Game.Goatee.Sgf.Property
import Game.Goatee.Sgf.TestInstances ()
import Game.Goatee.Sgf.TestUtils
import Game.Goatee.Sgf.Types
import Game.Goatee.Test.Common
import Test.HUnit ((~:), (@=?), (@?=), Test (TestList))

{-# ANN module "HLint: ignore Reduce duplication" #-}

type LoggedGoM = GoT (Writer [String])

runLoggedGo :: LoggedGoM a -> Cursor -> (a, Cursor, [String])
runLoggedGo go cursor =
  let ((value, cursor'), log) = runWriter $ runGoT go cursor
  in (value, cursor', log)

tests = "Game.Goatee.Sgf.Monad" ~: TestList [
  monadTests,
  navigationTests,
  positionStackTests,
  propertiesTests,
  getPropertyTests,
  getPropertyValueTests,
  putPropertyTests,
  deletePropertyTests,
  modifyPropertyTests,
  modifyPropertyValueTests,
  modifyPropertyStringTests,
  modifyPropertyCoordsTests,
  modifyGameInfoTests,
  modifyVariationModeTests,
  getMarkTests,
  modifyMarkTests,
  addChildTests,
  gameInfoChangedTests
  ]

monadTests = "monad properties" ~: TestList [
  "returns a value it's given" ~:
    let cursor = rootCursor $ node []
        (value, cursor') = runGo (return 3) cursor
    in (value, cursorNode cursor') @?= (3, cursorNode cursor),

  "getCursor works" ~: do
    let cursor = rootCursor nodeA
        nodeA = node1 [SZ 3 3, B $ Just (0,0)] nodeB
        nodeB = node1 [W $ Just (1,1)] nodeC
        nodeC = node [B $ Just (2,2)]
    nodeA @=? evalGo (liftM cursorNode getCursor) cursor
    nodeB @=? evalGo (goDown 0 >> liftM cursorNode getCursor) cursor
    nodeC @=? evalGo (goDown 0 >> goDown 0 >> liftM cursorNode getCursor) cursor,

  "getCoordState works" ~: do
    let cursor = child 0 $ child 0 $ rootCursor $
                 node1 [SZ 2 2, B $ Just (0,0)] $
                 node1 [W $ Just (1,0), TR $ coord1 (0,0)] $
                 node [B $ Just (0,1), CR $ coord1 (1,0)]
        action = mapM getCoordState [(0,0), (1,0), (0,1), (1,1)]
        states = evalGo action cursor
    map coordStone states @?= [Just Black, Just White, Just Black, Nothing]
    map coordMark states @?= [Nothing, Just MarkCircle, Nothing, Nothing]
  ]

navigationTests = "navigation" ~: TestList [
  "navigates down a tree" ~:
    let cursor = rootCursor $
                 node1 [B $ Just (0,0)] $
                 node' [W $ Just (1,1)] [node [B $ Just (2,2)],
                                         node [B Nothing]]
        action = goDown 0 >> goDown 1
        (_, cursor') = runGo action cursor
    in cursorProperties cursor' @?= [B Nothing],

  "navigates up a tree" ~:
    let cursor = child 1 $ child 0 $ rootCursor $
                 node1 [B $ Just (0,0)] $
                 node' [W $ Just (1,1)] [node [B $ Just (2,2)],
                                         node [B Nothing]]
        action = goUp >> goUp
        (_, cursor') = runGo action cursor
    in cursorProperties cursor' @?= [B $ Just (0,0)],

  "invokes handlers when navigating" ~:
    let cursor = rootCursor $ node1 [B Nothing] $ node [W Nothing]
        action = do on navigationEvent $ \step -> case step of
                      GoUp index -> tell ["Up " ++ show index]
                      _ -> return ()
                    on navigationEvent $ \step -> case step of
                      GoDown index -> tell ["Down " ++ show index]
                      _ -> return ()
                    goDown 0
                    goUp
        (_, _, log) = runLoggedGo action cursor
    in log @?= ["Down 0", "Up 0"],

  "navigates to the root of a tree, invoking handlers" ~: do
    let cursor = child 0 $ child 0 $ rootCursor $
                 node1 [B $ Just (0,0)] $
                 node1 [W $ Just (1,1)] $
                 node [B $ Just (2,2)]
        action = do on navigationEvent $ \step -> tell [show step]
                    goToRoot
        (_, cursor', log) = runLoggedGo action cursor
    cursorProperties cursor' @?= [B $ Just (0,0)]
    log @?= ["GoUp 0", "GoUp 0"],

  "goToGameInfoNode" ~: TestList [
    "can navigate up to find game info" ~:
      let props = [PB (toSimpleText ""), B Nothing]
          cursor = child 0 $ rootCursor $ node1 props $ node [W Nothing]
          action = void $ goToGameInfoNode False
      in cursorProperties (execGo action cursor) @?= props,

    "can find game info at the current node" ~:
      let props = [PB (toSimpleText ""), W Nothing]
          cursor = child 0 $ rootCursor $ node1 [B Nothing] $ node props
          action = void $ goToGameInfoNode False
      in cursorProperties (execGo action cursor) @?= props,

    "doesn't find game info from a subnode" ~:
      let cursor = child 0 $ rootCursor $
                   node1 [B $ Just (0,0)] $
                   node1 [W $ Just (1,1)] $
                   node [PB (toSimpleText ""), B Nothing]
          action = void $ goToGameInfoNode False
      in cursorProperties (execGo action cursor) @?= [W $ Just (1,1)],

    "can return to the initial node when no game info is found" ~:
      let cursor = child 0 $ child 0 $ rootCursor $
                   node1 [B $ Just (0,0)] $
                   node1 [W $ Just (1,1)] $
                   node [B $ Just (2,2)]
          action = void $ goToGameInfoNode False
      in cursorProperties (execGo action cursor) @?= [B $ Just (2,2)],

    "can finish at the root node when no game info is found" ~:
      let cursor = child 0 $ child 0 $ rootCursor $
                   node1 [B $ Just (0,0)] $
                   node1 [W $ Just (1,1)] $
                   node [B $ Just (2,2)]
          action = void $ goToGameInfoNode True
      in cursorProperties (execGo action cursor) @?= [B $ Just (0,0)]
    ]
  ]

positionStackTests = "position stack" ~: TestList [
  "should push, pop, and drop with no navigation" ~: do
    let cursor = rootCursor $ node []
        actions = [pushPosition >> popPosition,
                   pushPosition >> pushPosition >> popPosition >> popPosition,
                   pushPosition >> dropPosition,
                   pushPosition >> pushPosition >> dropPosition >> popPosition,
                   pushPosition >> pushPosition >> popPosition >> dropPosition]
    forM_ actions $ \action -> cursorProperties (execGo action cursor) @?= [],

  "should backtrack up and down the tree" ~: do
    let cursor = child 0 $ child 1 $ rootCursor $
                 node' [B $ Just (0,0)]
                       [node1 [W $ Just (1,1)] $ node [B $ Just (2,2)],
                        node1 [W Nothing] $ node [B Nothing]]
        action = pushPosition >> goUp >> goUp >> goDown 0 >> goDown 0
    cursorProperties (execGo (action >> popPosition) cursor) @?= [B Nothing]
    cursorProperties (execGo (action >> dropPosition) cursor) @?= [B $ Just (2,2)],

  "should pop multiple stacks" ~: do
    let cursor = child 0 $ child 0 commonCursor
        action = do navigate
                    log >> popPosition
                    log >> popPosition
                    log
    execWriter (runGoT action cursor) @?=
      ["B (2,2)", "B (3,3)", "B (5,5)", "B (3,3)", "B (2,2)"],

  "should drop then pop" ~: do
    let cursor = child 0 $ child 0 commonCursor
        action = do navigate
                    log >> dropPosition
                    log >> popPosition
                    log
    execWriter (runGoT action cursor) @?=
      ["B (2,2)", "B (3,3)", "B (5,5)", "B (5,5)", "B (2,2)"],

  "should drop twice" ~: do
    let cursor = child 0 $ child 0 commonCursor
        action = do navigate
                    log >> dropPosition
                    log >> dropPosition
                    log
    execWriter (runGoT action cursor) @?=
      ["B (2,2)", "B (3,3)", "B (5,5)", "B (5,5)", "B (5,5)"],

  "should fire navigation handlers while popping" ~: do
    let cursor = rootCursor $ node1 [B Nothing] $ node [W Nothing]
        action = do pushPosition
                    goDown 0
                    goUp
                    on navigationEvent $ \step -> tell [step]
                    popPosition
    execWriter (runGoT action cursor) @?= [GoDown 0, GoUp 0]
  ]
  where commonCursor = rootCursor $
                       node' [B $ Just (0,0)]
                             [node' [W $ Just (1,1)]
                                    [node [B $ Just (2,2)],
                                     node [B $ Just (3,3)]],
                              node1 [W $ Just (4,4)] $ node [B $ Just (5,5)]]
        log = getCursor >>= \cursor -> case cursorProperties cursor of
          [B (Just x)] -> tell ["B " ++ show x]
          [W (Just x)] -> tell ["W " ++ show x]
          xs -> error $ "Unexpected properties: " ++ show xs
        navigate = do log >> pushPosition
                      goUp >> goDown 1
                      log >> pushPosition
                      goUp >> goUp >> goDown 1 >> goDown 0

propertiesTests = "properties" ~: TestList [
  "getProperties" ~: TestList [
    "returns an empty list" ~:
      let cursor = rootCursor $ node []
      in evalGo getProperties cursor @?= [],

    "returns a non-empty list" ~:
      let properties = [PB $ toSimpleText "Foo", B Nothing]
          cursor = rootCursor $ node properties
      in evalGo getProperties cursor @?= properties
    ],

  "modifyProperties" ~: TestList [
    "adds properties" ~:
      let cursor = rootCursor $ node [FF 1]
          action = do modifyProperties $ \props -> return $ props ++ [B Nothing, W Nothing]
                      getProperties
      in evalGo action cursor @?= [FF 1, B Nothing, W Nothing],

    "removes properties" ~:
      let cursor = rootCursor $ node [W Nothing, FF 1, B Nothing]
          action = do modifyProperties $ \props -> return $ filter (not . isMoveProperty) props
                      getProperties
      in evalGo action cursor @?= [FF 1],

    "fires a properties modified event" ~:
      let cursor = rootCursor $ node [FF 1]
          action = do on propertiesModifiedEvent $ \old new -> tell [(old, new)]
                      modifyProperties $ const $ return [FF 2]
          log = execWriter (runGoT action cursor)
      in log @?= [([FF 1], [FF 2])]
    ]
  ]
  where isMoveProperty prop = case prop of
          B _ -> True
          W _ -> True
          _ -> False

getPropertyTests = "getProperty" ~: TestList [
  "doesn't find an unset property" ~: do
    Nothing @=? evalGo (getProperty propertyW) (rootCursor $ node [])
    Nothing @=? evalGo (getProperty propertyW) (rootCursor $ node [B Nothing]),

  "finds a set property" ~: do
    Just (B Nothing) @=? evalGo (getProperty propertyB) (rootCursor $ node [B Nothing])
    Just (B Nothing) @=? evalGo (getProperty propertyB) (rootCursor $ node [B Nothing, DO])
    Just DO @=? evalGo (getProperty propertyDO) (rootCursor $ node [B Nothing, DO])
  ]

getPropertyValueTests = "getPropertyValue" ~: TestList [
  "doesn't find an unset property" ~: do
    Nothing @=? evalGo (getPropertyValue propertyW) (rootCursor $ node [])
    Nothing @=? evalGo (getPropertyValue propertyW) (rootCursor $ node [B Nothing]),

  "finds a set property" ~: do
    Just Nothing @=? evalGo (getPropertyValue propertyB) (rootCursor $ node [B Nothing])
    Just (Just (0,0)) @=?
      evalGo (getPropertyValue propertyB) (rootCursor $ node [B $ Just (0,0), TE Double1])
    Just Double1 @=?
      evalGo (getPropertyValue propertyTE) (rootCursor $ node [B $ Just (0,0), TE Double1])
  ]

putPropertyTests = "putProperty" ~: TestList [
  "adds an unset property" ~: do
    [IT] @=? cursorProperties (execGo (putProperty IT) $ rootCursor $ node [])
    [DO, IT] @=?
      sortProperties (cursorProperties (execGo (putProperty IT) $ rootCursor $ node [DO])),

  "replaces an existing property" ~:
    [TE Double2] @=?
    cursorProperties (execGo (putProperty $ TE Double2) (rootCursor $ node [TE Double1]))
  ]

deletePropertyTests = "deleteProperty" ~: TestList [
  "does nothing when the property isn't set" ~: do
    [] @=? cursorProperties (execGo (deleteProperty DO) $ rootCursor $ node [])
    [B Nothing] @=? cursorProperties (execGo (deleteProperty DO) $ rootCursor $ node [B Nothing]),

  "removes a property" ~: do
    [] @=? cursorProperties (execGo (deleteProperty DO) $ rootCursor $ node [DO])
    -- This is documented behaviour for deleteProperty, the fact that it doesn't
    -- matter what the property value is:
    [DO] @=? cursorProperties
      (execGo (deleteProperty $ B $ Just (0,0)) $ rootCursor $ node [DO, B Nothing])
  ]

modifyPropertyTests = "modifyProperty" ~: TestList [
  "adds a property to an empty node" ~:
    let cursor = rootCursor $ node []
        action = modifyProperty propertyIT (\Nothing -> Just IT)
    in cursorProperties (execGo action cursor) @?= [IT],

  "adds a property to a non-empty node" ~:
    let cursor = rootCursor $ node [KO]
        action = modifyProperty propertyIT (\Nothing -> Just IT)
    in sortProperties (cursorProperties $ execGo action cursor) @?= [IT, KO],

  "removes a property" ~: do
    let cursor = rootCursor $ node [IT, KO]
        cursor' = execGo (modifyProperty propertyKO $ \(Just KO) -> Nothing) cursor
        cursor'' = execGo (modifyProperty propertyIT $ \(Just IT) -> Nothing) cursor'
    cursorProperties cursor' @?= [IT]
    cursorProperties cursor'' @?= [],

  "updates a property" ~: do
    let cursor = rootCursor $ node [B $ Just (0,0), BM Double2]
        action = modifyProperty propertyB $ \(Just (B (Just (0,0)))) -> Just $ B $ Just (1,1)
        cursor' = execGo action cursor
        action' = modifyProperty propertyBM $ \(Just (BM Double2)) -> Just $ BM Double1
        cursor'' = execGo action' cursor'
    sortProperties (cursorProperties cursor') @?= [B $ Just (1,1), BM Double2]
    sortProperties (cursorProperties cursor'') @?= [B $ Just (1,1), BM Double1],

  "fires an event when modifying a property" ~: do
    let cursor = rootCursor $ node [B $ Just (0,0)]
        action = do on propertiesModifiedEvent $ \[B (Just (0,0))] [] -> tell [0]
                    modifyProperty propertyB $ \(Just (B (Just (0,0)))) -> Nothing
        (cursor', log) = runWriter (execGoT action cursor)
    [0] @=? log
    [] @=? cursorProperties cursor',

  "modifying but not actually changing a property doesn't fire an event" ~: do
    let cursor = rootCursor $ node [B $ Just (0,0)]
        action = do on propertiesModifiedEvent $ \_ _ -> tell ["Event fired."]
                    modifyProperty propertyB $ \p@(Just (B {})) -> p
        (cursor', log) = runWriter (execGoT action cursor)
    [B $ Just (0,0)] @=? cursorProperties cursor'
    [] @=? log
  ]

modifyPropertyValueTests = "modifyPropertyValue" ~: TestList [
  "adds a property to an empty node" ~:
    let cursor = rootCursor $ node []
        action = modifyPropertyValue propertyPL (\Nothing -> Just Black)
    in cursorProperties (execGo action cursor) @?= [PL Black],

  "adds a property to a non-empty node" ~:
    let cursor = rootCursor $ node [KO]
        action = modifyPropertyValue propertyPL (\Nothing -> Just White)
    in sortProperties (cursorProperties $ execGo action cursor) @?= [KO, PL White],

  "removes a property" ~: do
    let cursor = rootCursor $ node [B Nothing, TE Double2]
        cursor' = execGo (modifyPropertyValue propertyTE $ \(Just Double2) -> Nothing) cursor
        cursor'' = execGo (modifyPropertyValue propertyB $ \(Just Nothing) -> Nothing) cursor'
    cursorProperties cursor' @?= [B Nothing]
    cursorProperties cursor'' @?= [],

  "updates a property" ~: do
    let cursor = rootCursor $ node [B $ Just (0,0), BM Double2]
        action = modifyPropertyValue propertyB $ \(Just (Just (0,0))) -> Just $ Just (1,1)
        cursor' = execGo action cursor
        action' = modifyPropertyValue propertyBM $ \(Just Double2) -> Just Double1
        cursor'' = execGo action' cursor'
    sortProperties (cursorProperties cursor') @?= [B $ Just (1,1), BM Double2]
    sortProperties (cursorProperties cursor'') @?= [B $ Just (1,1), BM Double1]
  ]

modifyPropertyStringTests =
  "modifyPropertyString" ~: TestList $
  -- Test a Text property and a SimpleText property.
  assumptions ++ genTests "comment" propertyC ++ genTests "game name" propertyGN
  where assumptions = let _ = C $ toText ""
                          _ = GN $ toSimpleText ""
                      in []
        genTests name property = [
          "adds a " ++ name ~:
            let cursor = rootCursor $ node []
                action = modifyPropertyString property (++ "Hello.")
            in cursorProperties (execGo action cursor) @?=
               [propertyBuilder property $ stringToSgf "Hello."],

          "removes a " ++ name ~:
            let cursor = rootCursor $ node [propertyBuilder property $ stringToSgf "Hello."]
                action = modifyPropertyString property $ \value -> case value of
                  "Hello." -> ""
                  other -> error $ "Got: " ++ other
            in cursorProperties (execGo action cursor) @?= [],

          "updates a " ++ name ~:
            let cursor = child 0 $ rootCursor $
                         node1 [propertyBuilder property $ stringToSgf "one"] $
                         node [propertyBuilder property $ stringToSgf "two"]
                action = modifyPropertyString property $ \value -> case value of
                  "two" -> "three"
                  other -> error $ "Got: " ++ other
                cursor' = execGo action cursor
            in (cursorProperties cursor', cursorProperties $ fromJust $ cursorParent cursor') @?=
               ([propertyBuilder property $ stringToSgf "three"],
                [propertyBuilder property $ stringToSgf "one"]),

          "leaves a non-existant comment" ~:
            let result = execGo (modifyPropertyString property id) $ rootCursor $ node []
            in cursorProperties result @?= []
          ]

modifyPropertyCoordsTests = "modifyPropertyCoords" ~: TestList [
  "adds a property where there was none" ~:
    let cursor = rootCursor $ node []
        action = modifyPropertyCoords propertySL $ \[] -> [(0,0), (1,1)]
    in [SL $ coords [(0,0), (1,1)]] @=? cursorProperties (execGo action cursor),

  "removes a property where there was one" ~:
    let cursor = rootCursor $ node [TR $ coord1 (5,5)]
        action = modifyPropertyCoords propertyTR $ \[(5,5)] -> []
    in [] @=? cursorProperties (execGo action cursor),

  "modifies an existing property" ~:
    let cursor = rootCursor $ node [CR $ coord1 (3,4)]
        action = modifyPropertyCoords propertyCR $ \[(3,4)] -> [(2,2)]
    in [CR $ coord1 (2,2)] @=? cursorProperties (execGo action cursor),

  "doesn't affect other properties" ~:
    let cursor = rootCursor $ node [SQ $ coord1 (0,0), MA $ coord1 (1,1)]
        action = modifyPropertyCoords propertyMA $ \[(1,1)] -> [(2,2)]
    in [MA $ coord1 (2,2), SQ $ coord1 (0,0)] @=?
       sortProperties (cursorProperties $ execGo action cursor)
  ]

modifyGameInfoTests = "modifyGameInfo" ~: TestList [
  "creates info on the root node, starting with none" ~: TestList [
     "starting from the root node" ~:
       let cursor = rootCursor $ node []
           action = modifyGameInfo $ \info ->
             info { gameInfoGameName = Just $ toSimpleText "Orange" }
       in cursorProperties (execGo action cursor) @?= [GN $ toSimpleText "Orange"],

     "starting from a non-root node" ~:
       let cursor = child 0 $ rootCursor $ node1 [] $ node [B Nothing]
           action = modifyGameInfo $ \info ->
             info { gameInfoGameName = Just $ toSimpleText "Orange" }
           cursor' = execGo action cursor
       in (cursorProperties cursor', cursorProperties $ fromJust $ cursorParent cursor') @?=
          ([B Nothing], [GN $ toSimpleText "Orange"])
     ],

  "modifies existing info on the root" ~: TestList [
    "starting from the root node" ~:
      let cursor = rootCursor $ node [GN $ toSimpleText "Orange"]
          action = modifyGameInfo (\info -> info { gameInfoGameName = Nothing
                                                 , gameInfoBlackName =
                                                   Just $ toSimpleText "Peanut butter"
                                                 })
      in cursorProperties (execGo action cursor) @?= [PB $ toSimpleText "Peanut butter"],

    "starting from a non-root node" ~:
      let cursor = child 0 $ rootCursor $ node1 [GN $ toSimpleText "Orange"] $ node [B Nothing]
          action = modifyGameInfo (\info -> info { gameInfoGameName = Nothing
                                                 , gameInfoBlackName =
                                                   Just $ toSimpleText "Peanut butter"
                                                 })
          cursor' = execGo action cursor
      in (cursorProperties cursor', cursorProperties $ fromJust $ cursorParent cursor') @?=
         ([B Nothing], [PB $ toSimpleText "Peanut butter"])
    ],

  "moves game info from a non-root node to the root" ~:
    let cursor = child 0 $ child 0 $ child 0 $ rootCursor $
                 node1 [SZ 19 19] $
                 node1 [B $ Just (0,0), GN $ toSimpleText "Orange"] $
                 node1 [W $ Just (1,1)] $
                 node [B $ Just (2,2)]
        action = modifyGameInfo (\info -> info { gameInfoGameName = Nothing })
        cursor' = execGo action cursor
        action' = modifyGameInfo (\info -> info { gameInfoBlackName =
                                                  Just $ toSimpleText "Peanut butter" })
        cursor'' = execGo action' cursor'
        -- unfoldr :: (Maybe Cursor -> Maybe ([Property], Maybe Cursor))
        --         -> Maybe Cursor
        --         -> [[Property]]
        properties = unfoldr (fmap $ cursorProperties &&& cursorParent)
                             (Just cursor'')
    in map sortProperties properties @?=
       [[B $ Just (2,2)],
        [W $ Just (1,1)],
        [B $ Just (0,0)],
        [PB $ toSimpleText "Peanut butter", SZ 19 19]]
  ]

modifyVariationModeTests = "modifyVariationMode" ~: TestList [
  "testing modes are not default" ~: do
    defaultVariationMode @/=? mode
    defaultVariationMode @/=? mode2,

  "modifies when at a root node" ~:
    node1 [ST mode] (node []) @=?
    cursorNode (execGo setMode $ rootCursor $ node1 [] $ node []),

  "modifies when at a non-root node" ~:
    node1 [ST mode] (node []) @=?
    cursorNode (cursorRoot $ execGo setMode $ child 0 $ rootCursor $ node1 [] $ node []),

  "leaves an unset ST unset" ~:
    assertST Nothing Nothing $ const defaultVariationMode,

  "leaves a default ST alone" ~:
    assertST (Just defaultVariationMode) (Just defaultVariationMode) $ const defaultVariationMode,

  "leaves an existing ST alone" ~:
    assertST (Just mode) (Just mode) $ const mode,

  "adds an ST property" ~:
    assertST (Just mode) Nothing $ const mode,

  "removes an ST property when setting to default" ~:
    assertST Nothing (Just mode) $ const defaultVariationMode,

  "modifies an existing ST property" ~:
    assertST (Just mode2) (Just mode) $ const mode2
  ]
  where mode = VariationMode ShowCurrentVariations True
        mode2 = VariationMode ShowChildVariations False
        setMode = modifyVariationMode $ const mode
        assertST maybeExpectedST maybeInitialST fn =
          node (ST <$> maybeToList maybeExpectedST) @=?
          cursorNode (execGo (modifyVariationMode fn) $
                      rootCursor $ node $ ST <$> maybeToList maybeInitialST)

getMarkTests = "getMark" ~: TestList [
  "returns Nothing for no mark" ~: do
    Nothing @=? evalGo (getMark (0,0)) (rootCursor $ node [])
    Nothing @=? evalGo (getMark (0,0)) (rootCursor $ node [W $ Just (0,0)]),

  "returns Just when a mark is present" ~: do
    Just MarkSquare @=? evalGo (getMark (1,2)) (rootCursor $ node [SQ $ coord1 (1,2)])
    Just MarkTriangle @=? evalGo (getMark (1,1)) (rootCursor $ node [B $ Just (1,1),
                                                                     TR $ coord1 (1,1),
                                                                     SQ $ coord1 (1,2)]),

  "matches all marks" ~: forM_ [minBound..maxBound] $ \mark ->
    Just mark @=?
    evalGo (getMark (0,0))
           (rootCursor $ node [propertyBuilder (markProperty mark) $ coord1 (0,0)])
  ]

modifyMarkTests = "modifyMark" ~: TestList [
  "creates a mark where there was none" ~:
    let cursor = rootCursor $ node []
        action = do modifyMark (\Nothing -> Just MarkX) (0,0)
                    getMark (0,0)
    in Just MarkX @=? evalGo action cursor,

  "removes an existing mark" ~:
    let cursor = rootCursor $ node [CR $ coord1 (0,0)]
        action = do modifyMark (\(Just MarkCircle) -> Nothing) (0,0)
                    getMark (0,0)
    in Nothing @=? evalGo action cursor,

  "replaces an existing mark" ~:
    let cursor = rootCursor $ node [CR $ coord1 (0,0)]
        action = do modifyMark (\(Just MarkCircle) -> Just MarkX) (0,0)
                    getMark (0,0)
    in Just MarkX @=? evalGo action cursor,

  "adds on to an existing mark property" ~:
    let cursor = rootCursor $ node [MA $ coord1 (0,0)]
        action = do modifyMark (\Nothing -> Just MarkX) (1,0)
                    mapM getMark [(0,0), (1,0)]
    in [Just MarkX, Just MarkX] @=? evalGo action cursor,

  "removes from an existing mark property" ~:
    let cursor = rootCursor $ node [MA $ coords [(0,0), (1,0), (0,1)]]
        action = do modifyMark (\(Just MarkX) -> Nothing) (1,0)
                    modifyMark (\(Just MarkX) -> Nothing) (0,1)
                    mapM getMark [(0,0), (1,0), (0,1)]
    in [Just MarkX, Nothing, Nothing] @=? evalGo action cursor,

  "removes and adds at the same time" ~:
    let cursor = rootCursor $ node [MA $ coords [(0,0), (1,0), (0,1)]]
        action = do modifyMark (\(Just MarkX) -> Just MarkSquare) (0,0)
                    modifyMark (\(Just MarkX) -> Nothing) (0,1)
                    mapM getMark [(0,0), (1,0), (0,1)]
    in [Just MarkSquare, Just MarkX, Nothing] @=? evalGo action cursor
  ]

addChildTests = "addChild" ~: TestList [
  "adds an only child" ~:
    cursorNode (execGo (addChild 0 $ node [B Nothing]) (rootCursor $ node []))
    @?= node' [] [node [B Nothing]],

  "adds a first child" ~:
    cursorNode (execGo (addChild 0 $ node [B $ Just (0,0)])
                       (rootCursor $ node' [] [node [B $ Just (1,1)]]))
    @?= node' [] [node [B $ Just (0,0)],
                  node [B $ Just (1,1)]],

  "adds a middle child" ~:
    cursorNode (execGo (addChild 1 $ node [B $ Just (1,1)])
                       (rootCursor $ node' [] [node [B $ Just (0,0)],
                                               node [B $ Just (2,2)]]))
    @?= node' [] [node [B $ Just (0,0)],
                  node [B $ Just (1,1)],
                  node [B $ Just (2,2)]],

  "adds a last child" ~:
    cursorNode (execGo (addChild 2 $ node [B $ Just (2,2)])
                       (rootCursor $ node' [] [node [B $ Just (0,0)],
                                               node [B $ Just (1,1)]]))
    @?= node' [] [node [B $ Just (0,0)],
                  node [B $ Just (1,1)],
                  node [B $ Just (2,2)]],

  "path stack correctness" ~: TestList [
    "basic case just not needing updating" ~:
      let cursor = child 0 $ rootCursor $ node' [B Nothing] [node [W Nothing]]
          action = do pushPosition
                      goUp
                      addChild 1 $ node [W $ Just (0,0)]
                      popPosition
      in cursorNode (execGo action cursor) @?= node [W Nothing],

    "basic case just needing updating" ~:
      let cursor = child 0 $ rootCursor $ node' [B Nothing] [node [W Nothing]]
          action = do pushPosition
                      goUp
                      addChild 0 $ node [W $ Just (0,0)]
                      popPosition
      in cursorNode (execGo action cursor) @?= node [W Nothing],

    "basic case definitely needing updating" ~:
      let cursor = rootCursor $ node' [B Nothing] [node [W $ Just (0,0)],
                                                   node [W $ Just (1,1)]]
          action = do goDown 1
                      pushPosition
                      goUp
                      addChild 0 $ node [W Nothing]
                      popPosition
      in cursorNode (execGo action cursor) @?= node [W $ Just (1,1)],

    "multiple paths to update" ~:
      let at y x = B $ Just (y,x)
          level0Node = node' [at 0 0] $ map level1Node [0..1]
          level1Node i = node' [at 1 i] $ map level2Node [0..2]
          level2Node i = node' [at 2 i] $ map level3Node [0..3]
          level3Node i = node [at 3 i]
          action = do goDown 1 >> goDown 2 >> goDown 3
                      pushPosition
                      replicateM_ 4 goUp
                      goDown 1 >> goDown 2 >> goDown 2 >> goUp >> goDown 1
                      addChild 0 $ node' [] [node []]
                      goDown 0 >> goDown 0
                      goToRoot
                      popPosition
      in cursorNode (execGo action $ rootCursor level0Node) @?= node [B $ Just (3,3)],

    "updates paths with GoUp correctly" ~:
      let cursor = rootCursor $ node1 [B $ Just (0,0)] $ node [W $ Just (1,1)]
          action = do pushPosition
                      goDown 0
                      goUp
                      addChild 0 $ node [B $ Just (2,2)]
                      on navigationEvent $ \step -> tell [step]
                      popPosition
          (_, log) = runWriter (runGoT action cursor)
      in log @?= [GoDown 1, GoUp 1]
    ]
  ]

gameInfoChangedTests = "gameInfoChangedEvent" ~: TestList [
  "fires when navigating down" ~:
    let cursor = rootCursor $
                 node1 [B $ Just (0,0)] $
                 node [W $ Just (0,0), GN $ toSimpleText "Foo"]
        action = do on gameInfoChangedEvent onInfo
                    goDown 0
    in execWriter (runGoT action cursor) @?= [(Nothing, Just $ toSimpleText "Foo")],

  "fires when navigating up" ~:
    let cursor = child 0 $ rootCursor $
                 node1 [B $ Just (0,0)] $
                 node [W $ Just (0,0), GN $ toSimpleText "Foo"]
        action = do on gameInfoChangedEvent onInfo
                    goUp
    in execWriter (runGoT action cursor) @?= [(Just $ toSimpleText "Foo", Nothing)],

  "fires from within popPosition" ~:
    let cursor = rootCursor $
                 node1 [B $ Just (0,0)] $
                 node [W $ Just (0,0), GN $ toSimpleText "Foo"]
        action = do pushPosition
                    goDown 0
                    goUp
                    on gameInfoChangedEvent onInfo
                    popPosition
    in execWriter (runGoT action cursor) @?=
       [(Nothing, Just $ toSimpleText "Foo"), (Just $ toSimpleText "Foo", Nothing)],

  "fires when modifying properties" ~:
    let cursor = rootCursor $ node []
        action = do on gameInfoChangedEvent onInfo
                    modifyProperties $ const $ return [GN $ toSimpleText "Foo"]
                    modifyProperties $ const $ return [GN $ toSimpleText "Bar"]
                    modifyProperties $ const $ return []
    in execWriter (runGoT action cursor) @?=
       [(Nothing, Just $ toSimpleText "Foo"),
        (Just $ toSimpleText "Foo", Just $ toSimpleText "Bar"),
        (Just $ toSimpleText "Bar", Nothing)]
  ]
  where onInfo old new = tell [(gameInfoGameName old, gameInfoGameName new)]
