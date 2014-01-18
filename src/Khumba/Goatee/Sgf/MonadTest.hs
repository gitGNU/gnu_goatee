module Khumba.Goatee.Sgf.MonadTest (tests) where

import Control.Monad.Writer
import Khumba.Goatee.Sgf hiding (addChild)
import Khumba.Goatee.Sgf.Monad
import Khumba.Goatee.SgfTestUtils
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Node, Test)

type LoggedGoM = GoT (Writer [String])

runLoggedGo :: LoggedGoM a -> Cursor -> (a, Cursor, [String])
runLoggedGo go cursor =
  let ((value, cursor'), log) = runWriter $ runGoT go cursor
  in (value, cursor', log)

tests = testGroup "Khumba.Goatee.Sgf.Monad" [
  monadTests,
  navigationTests,
  positionStackTests,
  addChildTests
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
                      GoUp -> tell ["Up"]
                      _ -> return ()
                    on navigationEvent $ \step -> case step of
                      GoDown childIndex -> tell ["Down " ++ show childIndex]
                      _ -> return ()
                    goDown 0
                    goUp
        (_, _, log) = runLoggedGo action cursor
    in log @?= ["Down 0", "Up"],

  testCase "navigates to the root of a tree, invoking handlers" $ do
    let cursor = child 0 $ child 0 $ rootCursor $
                 node1 [B $ Just (0,0)] $
                 node1 [W $ Just (1,1)] $
                 node [B $ Just (2,2)]
        action = do on navigationEvent $ \step -> tell [show step]
                    goToRoot
        (_, cursor', log) = runLoggedGo action cursor
    cursorProperties cursor' @?= [B $ Just (0,0)]
    log @?= ["GoUp", "GoUp"],

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
      ["B (2,2)", "B (3,3)", "B (5,5)", "B (5,5)", "B (5,5)"]
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
      in cursorNode (execGo action $ rootCursor level0Node) @?= node [B $ Just (3,3)]
    ]
  ]
