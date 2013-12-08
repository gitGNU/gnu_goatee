module Khumba.GoHS.Sgf.MonadTest (tests) where

import Control.Monad.Writer
import Khumba.GoHS.Sgf
import Khumba.GoHS.Sgf.Monad
import Khumba.GoHS.SgfTestUtils
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Node, Test)

type LoggedGoM a = GoM (Writer [String]) a

tests = testGroup "Khumba.GoHS.Sgf.Monad" [
  monadTests,
  navigationTests,
  positionStackTests
  ]

monadTests = testGroup "monad properties" [
  testCase "returns the value it's given" $
    let cursor = rootCursor $ node []
        (value, cursor', _) = runGo (return 3 :: LoggedGoM Int) cursor
    in (value, cursorNode cursor') @?= (3, cursorNode cursor)
  ]

navigationTests = testGroup "navigation" [
  testCase "navigates down a tree" $
    let cursor = rootCursor $
                 node1 [B $ Just (0,0)] $
                 node' [W $ Just (1,1)] [node [B $ Just (2,2)],
                                         node [B Nothing]]
        action = goDown 0 >> goDown 1 :: LoggedGoM ()
        (_, cursor', _) = runGo action cursor
    in cursorProperties cursor' @?= [B Nothing],

  testCase "navigates up a tree" $
    let cursor = child 1 $ child 0 $ rootCursor $
                 node1 [B $ Just (0,0)] $
                 node' [W $ Just (1,1)] [node [B $ Just (2,2)],
                                         node [B Nothing]]
        action = goUp >> goUp :: LoggedGoM ()
        (_, cursor', _) = runGo action cursor
    in cursorProperties cursor' @?= [B $ Just (0,0)],

  testCase "invokes handlers when navigating" $
    let cursor = rootCursor $ node1 [B Nothing] $ node [W Nothing]
        action = do on navigationEvent $ \step -> case step of
                      GoUp -> tell ["Up"] >> return False
                      _ -> return False
                    on navigationEvent $ \step -> case step of
                      GoDown childIndex -> do tell ["Down " ++ show childIndex]
                                              return False
                      _ -> return False
                    goDown 0
                    goUp
        (_, _, writer) = runGo action cursor
        result = execWriter writer
    in result @?= ["Down 0", "Up"],

  testCase "navigates to the root of a tree, invoking handlers" $ do
    let cursor = child 0 $ child 0 $ rootCursor $
                 node1 [B $ Just (0,0)] $
                 node1 [W $ Just (1,1)] $
                 node [B $ Just (2,2)]
        action = do on navigationEvent $ \step -> tell [show step] >> return False
                    goToRoot
        (_, cursor', writer) = runGo action cursor
        log = execWriter writer
    cursorProperties cursor' @?= [B $ Just (0,0)]
    log @?= ["GoUp", "GoUp"],

  testGroup "goToGameInfoNode" [
    testCase "can navigate up to find game info" $
      let props = [PB (toSimpleText ""), B Nothing]
          cursor = child 0 $ rootCursor $ node1 props $ node [W Nothing]
          action = void $ goToGameInfoNode False :: BasicGoM ()
      in cursorProperties (execGo action cursor) @?= props,

    testCase "can find game info at the current node" $
      let props = [PB (toSimpleText ""), W Nothing]
          cursor = child 0 $ rootCursor $ node1 [B Nothing] $ node props
          action = void $ goToGameInfoNode False :: BasicGoM ()
      in cursorProperties (execGo action cursor) @?= props,

    testCase "doesn't find game info from a subnode" $
      let cursor = child 0 $ rootCursor $
                   node1 [B $ Just (0,0)] $
                   node1 [W $ Just (1,1)] $
                   node [PB (toSimpleText ""), B Nothing]
          action = void $ goToGameInfoNode False :: BasicGoM ()
      in cursorProperties (execGo action cursor) @?= [W $ Just (1,1)],

    testCase "can return to the initial node when no game info is found" $
      let cursor = child 0 $ child 0 $ rootCursor $
                   node1 [B $ Just (0,0)] $
                   node1 [W $ Just (1,1)] $
                   node [B $ Just (2,2)]
          action = void $ goToGameInfoNode False :: BasicGoM ()
      in cursorProperties (execGo action cursor) @?= [B $ Just (2,2)],

    testCase "can finish at the root node when no game info is found" $
      let cursor = child 0 $ child 0 $ rootCursor $
                   node1 [B $ Just (0,0)] $
                   node1 [W $ Just (1,1)] $
                   node [B $ Just (2,2)]
          action = void $ goToGameInfoNode True :: BasicGoM ()
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
                   pushPosition >> pushPosition >> popPosition >> dropPosition] :: [BasicGoM ()]
    forM_ actions $ \action -> cursorProperties (execGo action cursor) @?= [],

  testCase "should backtrack up and down the tree" $ do
    let cursor = child 0 $ child 1 $ rootCursor $
                 node' [B $ Just (0,0)]
                       [node1 [W $ Just (1,1)] $ node [B $ Just (2,2)],
                        node1 [W Nothing] $ node [B Nothing]]
        action = pushPosition >> goUp >> goUp >> goDown 0 >> goDown 0 :: BasicGoM ()
    cursorProperties (execGo (action >> popPosition) cursor) @?= [B Nothing]
    cursorProperties (execGo (action >> dropPosition) cursor) @?= [B $ Just (2,2)],

  testCase "should pop multiple stacks" $ do
    let cursor = child 0 $ child 0 commonCursor
        action = do navigate
                    log >> popPosition
                    log >> popPosition
                    log
    execWriter (runBasicGoT action cursor) @?=
      ["B (2,2)", "B (3,3)", "B (5,5)", "B (3,3)", "B (2,2)"],

  testCase "should drop then pop" $ do
    let cursor = child 0 $ child 0 commonCursor
        action = do navigate
                    log >> dropPosition
                    log >> popPosition
                    log
    execWriter (runBasicGoT action cursor) @?=
      ["B (2,2)", "B (3,3)", "B (5,5)", "B (5,5)", "B (2,2)"],

  testCase "should drop twice" $ do
    let cursor = child 0 $ child 0 commonCursor
        action = do navigate
                    log >> dropPosition
                    log >> dropPosition
                    log
    execWriter (runBasicGoT action cursor) @?=
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
        navigate = do log >> pushPosition
                      goUp >> goDown 1
                      log >> pushPosition
                      goUp >> goUp >> goDown 1 >> goDown 0
