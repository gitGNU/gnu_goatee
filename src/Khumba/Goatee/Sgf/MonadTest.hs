module Khumba.Goatee.Sgf.MonadTest (tests) where

import Control.Arrow ((&&&))
import Control.Monad.Writer
import Data.List (unfoldr)
import Data.Maybe
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Monad
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.TestUtils
import Khumba.Goatee.Sgf.Types
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Node, Test)

{-# ANN module "HLint: ignore Reduce duplication" #-}

type LoggedGoM = GoT (Writer [String])

runLoggedGo :: LoggedGoM a -> Cursor -> (a, Cursor, [String])
runLoggedGo go cursor =
  let ((value, cursor'), log) = runWriter $ runGoT go cursor
  in (value, cursor', log)

tests = testGroup "Khumba.Goatee.Sgf.Monad" [
  monadTests,
  navigationTests,
  positionStackTests,
  propertiesTests,
  modifyGameInfoTests,
  modifyPropertyTests,
  modifyPropertyValueTests,
  modifyPropertyStringTests,
  addChildTests,
  gameInfoChangedTests
  ]

monadTests = testGroup "monad properties" [
  testCase "returns a value it's given" $
    let cursor = rootCursor $ node []
        (value, cursor') = runGo (return 3) cursor
    in (value, cursorNode cursor') @?= (3, cursorNode cursor)
  ]

navigationTests = testGroup "navigation" [
  testCase "navigates down a tree" $
    let cursor = rootCursor $
                 node1 [B $ Just (0,0)] $
                 node' [W $ Just (1,1)] [node [B $ Just (2,2)],
                                         node [B Nothing]]
        action = goDown 0 >> goDown 1
        (_, cursor') = runGo action cursor
    in cursorProperties cursor' @?= [B Nothing],

  testCase "navigates up a tree" $
    let cursor = child 1 $ child 0 $ rootCursor $
                 node1 [B $ Just (0,0)] $
                 node' [W $ Just (1,1)] [node [B $ Just (2,2)],
                                         node [B Nothing]]
        action = goUp >> goUp
        (_, cursor') = runGo action cursor
    in cursorProperties cursor' @?= [B $ Just (0,0)],

  testCase "invokes handlers when navigating" $
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

  testCase "navigates to the root of a tree, invoking handlers" $ do
    let cursor = child 0 $ child 0 $ rootCursor $
                 node1 [B $ Just (0,0)] $
                 node1 [W $ Just (1,1)] $
                 node [B $ Just (2,2)]
        action = do on navigationEvent $ \step -> tell [show step]
                    goToRoot
        (_, cursor', log) = runLoggedGo action cursor
    cursorProperties cursor' @?= [B $ Just (0,0)]
    log @?= ["GoUp 0", "GoUp 0"],

  testGroup "goToGameInfoNode" [
    testCase "can navigate up to find game info" $
      let props = [PB (toSimpleText ""), B Nothing]
          cursor = child 0 $ rootCursor $ node1 props $ node [W Nothing]
          action = void $ goToGameInfoNode False
      in cursorProperties (execGo action cursor) @?= props,

    testCase "can find game info at the current node" $
      let props = [PB (toSimpleText ""), W Nothing]
          cursor = child 0 $ rootCursor $ node1 [B Nothing] $ node props
          action = void $ goToGameInfoNode False
      in cursorProperties (execGo action cursor) @?= props,

    testCase "doesn't find game info from a subnode" $
      let cursor = child 0 $ rootCursor $
                   node1 [B $ Just (0,0)] $
                   node1 [W $ Just (1,1)] $
                   node [PB (toSimpleText ""), B Nothing]
          action = void $ goToGameInfoNode False
      in cursorProperties (execGo action cursor) @?= [W $ Just (1,1)],

    testCase "can return to the initial node when no game info is found" $
      let cursor = child 0 $ child 0 $ rootCursor $
                   node1 [B $ Just (0,0)] $
                   node1 [W $ Just (1,1)] $
                   node [B $ Just (2,2)]
          action = void $ goToGameInfoNode False
      in cursorProperties (execGo action cursor) @?= [B $ Just (2,2)],

    testCase "can finish at the root node when no game info is found" $
      let cursor = child 0 $ child 0 $ rootCursor $
                   node1 [B $ Just (0,0)] $
                   node1 [W $ Just (1,1)] $
                   node [B $ Just (2,2)]
          action = void $ goToGameInfoNode True
      in cursorProperties (execGo action cursor) @?= [B $ Just (0,0)]
    ]
  ]

positionStackTests = testGroup "position stack" [
  testCase "should push, pop, and drop with no navigation" $ do
    let cursor = rootCursor $ node []
        actions = [pushPosition >> popPosition,
                   pushPosition >> pushPosition >> popPosition >> popPosition,
                   pushPosition >> dropPosition,
                   pushPosition >> pushPosition >> dropPosition >> popPosition,
                   pushPosition >> pushPosition >> popPosition >> dropPosition]
    forM_ actions $ \action -> cursorProperties (execGo action cursor) @?= [],

  testCase "should backtrack up and down the tree" $ do
    let cursor = child 0 $ child 1 $ rootCursor $
                 node' [B $ Just (0,0)]
                       [node1 [W $ Just (1,1)] $ node [B $ Just (2,2)],
                        node1 [W Nothing] $ node [B Nothing]]
        action = pushPosition >> goUp >> goUp >> goDown 0 >> goDown 0
    cursorProperties (execGo (action >> popPosition) cursor) @?= [B Nothing]
    cursorProperties (execGo (action >> dropPosition) cursor) @?= [B $ Just (2,2)],

  testCase "should pop multiple stacks" $ do
    let cursor = child 0 $ child 0 commonCursor
        action = do navigate
                    log >> popPosition
                    log >> popPosition
                    log
    execWriter (runGoT action cursor) @?=
      ["B (2,2)", "B (3,3)", "B (5,5)", "B (3,3)", "B (2,2)"],

  testCase "should drop then pop" $ do
    let cursor = child 0 $ child 0 commonCursor
        action = do navigate
                    log >> dropPosition
                    log >> popPosition
                    log
    execWriter (runGoT action cursor) @?=
      ["B (2,2)", "B (3,3)", "B (5,5)", "B (5,5)", "B (2,2)"],

  testCase "should drop twice" $ do
    let cursor = child 0 $ child 0 commonCursor
        action = do navigate
                    log >> dropPosition
                    log >> dropPosition
                    log
    execWriter (runGoT action cursor) @?=
      ["B (2,2)", "B (3,3)", "B (5,5)", "B (5,5)", "B (5,5)"],

  testCase "should fire navigation handlers while popping" $ do
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

propertiesTests = testGroup "properties" [
  testGroup "getProperties" [
    testCase "returns an empty list" $
      let cursor = rootCursor $ node []
      in evalGo getProperties cursor @?= [],

    testCase "returns a non-empty list" $
      let properties = [PB $ toSimpleText "Foo", B Nothing]
          cursor = rootCursor $ node properties
      in evalGo getProperties cursor @?= properties
    ],

  testGroup "modifyProperties" [
    testCase "adds properties" $
      let cursor = rootCursor $ node [FF 1]
          action = do modifyProperties $ \props -> return $ props ++ [B Nothing, W Nothing]
                      getProperties
      in evalGo action cursor @?= [FF 1, B Nothing, W Nothing],

    testCase "removes properties" $
      let cursor = rootCursor $ node [W Nothing, FF 1, B Nothing]
          action = do modifyProperties $ \props -> return $ filter (not . isMoveProperty) props
                      getProperties
      in evalGo action cursor @?= [FF 1],

    testCase "fires a properties changed event" $
      let cursor = rootCursor $ node [FF 1]
          action = do on propertiesChangedEvent $ \old new -> tell [(old, new)]
                      modifyProperties $ const $ return [FF 2]
          log = execWriter (runGoT action cursor)
      in log @?= [([FF 1], [FF 2])]
    ],

  testGroup "deleteProperties" [
    testCase "leaves non-matching properties" $
      let cursor = rootCursor $ node [FF 4, GM 1]
          action = do deleteProperties isMoveProperty
                      getProperties
      in evalGo action cursor @?= [FF 4, GM 1],

    testCase "removes matching properties" $
      let cursor = rootCursor $ node [B Nothing, FF 4, GM 1, W $ Just (1,1)]
          action = do deleteProperties isMoveProperty
                      getProperties
      in evalGo action cursor @?= [FF 4, GM 1],

    testCase "fires a properties changed event" $
      let cursor = rootCursor $ node [B Nothing, FF 4, GM 1, W $ Just (1,1)]
          action = do on propertiesChangedEvent $ \old new -> tell [(old, new)]
                      deleteProperties isMoveProperty
                      getProperties
          (_, log) = runWriter (runGoT action cursor)
      in log @?= [([B Nothing, FF 4, GM 1, W $ Just (1,1)],
                   [FF 4, GM 1])]
    ]
  ]
  where isMoveProperty prop = case prop of
          B _ -> True
          W _ -> True
          _ -> False

modifyGameInfoTests = testGroup "modifyGameInfo" [
  testGroup "creates info on the root node, starting with none" [
     testCase "starting from the root node" $
       let cursor = rootCursor $ node []
           action = modifyGameInfo (\info -> info { gameInfoGameName = Just "Orange" })
       in cursorProperties (execGo action cursor) @?= [GN $ toSimpleText "Orange"],

     testCase "starting from a non-root node" $
       let cursor = child 0 $ rootCursor $ node1 [] $ node [B Nothing]
           action = modifyGameInfo (\info -> info { gameInfoGameName = Just "Orange" })
           cursor' = execGo action cursor
       in (cursorProperties cursor', cursorProperties $ fromJust $ cursorParent cursor') @?=
          ([B Nothing], [GN $ toSimpleText "Orange"])
     ],

  testGroup "modifies existing info on the root" [
    testCase "starting from the root node" $
      let cursor = rootCursor $ node [GN $ toSimpleText "Orange"]
          action = modifyGameInfo (\info -> info { gameInfoGameName = Nothing
                                                 , gameInfoBlackName = Just "Peanut butter"
                                                 })
      in cursorProperties (execGo action cursor) @?= [PB $ toSimpleText "Peanut butter"],

    testCase "starting from a non-root node" $
      let cursor = child 0 $ rootCursor $ node1 [GN $ toSimpleText "Orange"] $ node [B Nothing]
          action = modifyGameInfo (\info -> info { gameInfoGameName = Nothing
                                                 , gameInfoBlackName = Just "Peanut butter"
                                                 })
          cursor' = execGo action cursor
      in (cursorProperties cursor', cursorProperties $ fromJust $ cursorParent cursor') @?=
         ([B Nothing], [PB $ toSimpleText "Peanut butter"])
    ],

  testCase "moves game info from a non-root node to the root" $
    let cursor = child 0 $ child 0 $ child 0 $ rootCursor $
                 node1 [SZ 19 19] $
                 node1 [B $ Just (0,0), GN $ toSimpleText "Orange"] $
                 node1 [W $ Just (1,1)] $
                 node [B $ Just (2,2)]
        action = modifyGameInfo (\info -> info { gameInfoGameName = Nothing })
        cursor' = execGo action cursor
        action' = modifyGameInfo (\info -> info { gameInfoBlackName = Just "Peanut butter" })
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

modifyPropertyTests = testGroup "modifyProperty" [
  testCase "adds a property to an empty node" $
    let cursor = rootCursor $ node []
        action = modifyProperty propertyIT (\Nothing -> Just IT)
    in cursorProperties (execGo action cursor) @?= [IT],

  testCase "adds a property to a non-empty node" $
    let cursor = rootCursor $ node [KO]
        action = modifyProperty propertyIT (\Nothing -> Just IT)
    in sortProperties (cursorProperties $ execGo action cursor) @?= [IT, KO],

  testCase "removes a property" $ do
    let cursor = rootCursor $ node [IT, KO]
        cursor' = execGo (modifyProperty propertyKO $ \(Just KO) -> Nothing) cursor
        cursor'' = execGo (modifyProperty propertyIT $ \(Just IT) -> Nothing) cursor'
    cursorProperties cursor' @?= [IT]
    cursorProperties cursor'' @?= [],

  testCase "updates a property" $ do
    let cursor = rootCursor $ node [B $ Just (0,0), BM Double2]
        action = modifyProperty propertyB $ \(Just (B (Just (0,0)))) -> Just $ B $ Just (1,1)
        cursor' = execGo action cursor
        action' = modifyProperty propertyBM $ \(Just (BM Double2)) -> Just $ BM Double1
        cursor'' = execGo action' cursor'
    sortProperties (cursorProperties cursor') @?= [B $ Just (1,1), BM Double2]
    sortProperties (cursorProperties cursor'') @?= [B $ Just (1,1), BM Double1]
  ]

modifyPropertyValueTests = testGroup "modifyPropertyValue" [
  testCase "adds a property to an empty node" $
    let cursor = rootCursor $ node []
        action = modifyPropertyValue propertyPL (\Nothing -> Just Black)
    in cursorProperties (execGo action cursor) @?= [PL Black],

  testCase "adds a property to a non-empty node" $
    let cursor = rootCursor $ node [KO]
        action = modifyPropertyValue propertyPL (\Nothing -> Just White)
    in sortProperties (cursorProperties $ execGo action cursor) @?= [KO, PL White],

  testCase "removes a property" $ do
    let cursor = rootCursor $ node [B Nothing, TE Double2]
        cursor' = execGo (modifyPropertyValue propertyTE $ \(Just Double2) -> Nothing) cursor
        cursor'' = execGo (modifyPropertyValue propertyB $ \(Just Nothing) -> Nothing) cursor'
    cursorProperties cursor' @?= [B Nothing]
    cursorProperties cursor'' @?= [],

  testCase "updates a property" $ do
    let cursor = rootCursor $ node [B $ Just (0,0), BM Double2]
        action = modifyPropertyValue propertyB $ \(Just (Just (0,0))) -> Just $ Just (1,1)
        cursor' = execGo action cursor
        action' = modifyPropertyValue propertyBM $ \(Just Double2) -> Just Double1
        cursor'' = execGo action' cursor'
    sortProperties (cursorProperties cursor') @?= [B $ Just (1,1), BM Double2]
    sortProperties (cursorProperties cursor'') @?= [B $ Just (1,1), BM Double1]
  ]

modifyPropertyStringTests =
  testGroup "modifyPropertyString"
  -- Test a Text property and a SimpleText property.
  (assumptions ++ genTests "comment" propertyC ++ genTests "game name" propertyGN)
  where assumptions = let _ = C $ toText ""
                          _ = GN $ toSimpleText ""
                      in []
        genTests name property = [
          testCase ("adds a " ++ name) $
            let cursor = rootCursor $ node []
                action = modifyPropertyString property (++ "Hello.")
            in cursorProperties (execGo action cursor) @?=
               [propertyBuilder property $ stringToSgf "Hello."],

          testCase ("removes a " ++ name) $
            let cursor = rootCursor $ node [propertyBuilder property $ stringToSgf "Hello."]
                action = modifyPropertyString property $ \value -> case value of
                  "Hello." -> ""
                  other -> error $ "Got: " ++ other
            in cursorProperties (execGo action cursor) @?= [],

          testCase ("updates a " ++ name) $
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

          testCase "leaves a non-existant comment" $
            let result = execGo (modifyPropertyString property id) $ rootCursor $ node []
            in cursorProperties result @?= []
          ]

addChildTests = testGroup "addChild" [
  testCase "adds an only child" $
    cursorNode (execGo (addChild 0 $ node [B Nothing]) (rootCursor $ node []))
    @?= node' [] [node [B Nothing]],

  testCase "adds a first child" $
    cursorNode (execGo (addChild 0 $ node [B $ Just (0,0)])
                       (rootCursor $ node' [] [node [B $ Just (1,1)]]))
    @?= node' [] [node [B $ Just (0,0)],
                  node [B $ Just (1,1)]],

  testCase "adds a middle child" $
    cursorNode (execGo (addChild 1 $ node [B $ Just (1,1)])
                       (rootCursor $ node' [] [node [B $ Just (0,0)],
                                               node [B $ Just (2,2)]]))
    @?= node' [] [node [B $ Just (0,0)],
                  node [B $ Just (1,1)],
                  node [B $ Just (2,2)]],

  testCase "adds a last child" $
    cursorNode (execGo (addChild 2 $ node [B $ Just (2,2)])
                       (rootCursor $ node' [] [node [B $ Just (0,0)],
                                               node [B $ Just (1,1)]]))
    @?= node' [] [node [B $ Just (0,0)],
                  node [B $ Just (1,1)],
                  node [B $ Just (2,2)]],

  testGroup "path stack correctness" [
    testCase "basic case just not needing updating" $
      let cursor = child 0 $ rootCursor $ node' [B Nothing] [node [W Nothing]]
          action = do pushPosition
                      goUp
                      addChild 1 $ node [W $ Just (0,0)]
                      popPosition
      in cursorNode (execGo action cursor) @?= node [W Nothing],

    testCase "basic case just needing updating" $
      let cursor = child 0 $ rootCursor $ node' [B Nothing] [node [W Nothing]]
          action = do pushPosition
                      goUp
                      addChild 0 $ node [W $ Just (0,0)]
                      popPosition
      in cursorNode (execGo action cursor) @?= node [W Nothing],

    testCase "basic case definitely needing updating" $
      let cursor = rootCursor $ node' [B Nothing] [node [W $ Just (0,0)],
                                                   node [W $ Just (1,1)]]
          action = do goDown 1
                      pushPosition
                      goUp
                      addChild 0 $ node [W Nothing]
                      popPosition
      in cursorNode (execGo action cursor) @?= node [W $ Just (1,1)],

    testCase "multiple paths to update" $
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

    testCase "updates paths with GoUp correctly" $
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

gameInfoChangedTests = testGroup "gameInfoChangedEvent" [
  testCase "fires when navigating down" $
    let cursor = rootCursor $
                 node1 [B $ Just (0,0)] $
                 node [W $ Just (0,0), GN $ toSimpleText "Foo"]
        action = do on gameInfoChangedEvent onInfo
                    goDown 0
    in execWriter (runGoT action cursor) @?= [(Nothing, Just "Foo")],

  testCase "fires when navigating up" $
    let cursor = child 0 $ rootCursor $
                 node1 [B $ Just (0,0)] $
                 node [W $ Just (0,0), GN $ toSimpleText "Foo"]
        action = do on gameInfoChangedEvent onInfo
                    goUp
    in execWriter (runGoT action cursor) @?= [(Just "Foo", Nothing)],

  testCase "fires from within popPosition" $
    let cursor = rootCursor $
                 node1 [B $ Just (0,0)] $
                 node [W $ Just (0,0), GN $ toSimpleText "Foo"]
        action = do pushPosition
                    goDown 0
                    goUp
                    on gameInfoChangedEvent onInfo
                    popPosition
    in execWriter (runGoT action cursor) @?=
       [(Nothing, Just "Foo"), (Just "Foo", Nothing)],

  testCase "fires when modifying properties" $
    let cursor = rootCursor $ node []
        action = do on gameInfoChangedEvent onInfo
                    modifyProperties $ const $ return [GN $ toSimpleText "Foo"]
                    modifyProperties $ const $ return [GN $ toSimpleText "Bar"]
                    modifyProperties $ const $ return []
    in execWriter (runGoT action cursor) @?=
       [(Nothing, Just "Foo"), (Just "Foo", Just "Bar"), (Just "Bar", Nothing)]
  ]
  where onInfo old new = tell [(gameInfoGameName old, gameInfoGameName new)]
