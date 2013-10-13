module Khumba.GoHS.SgfTest (tests) where

import Khumba.GoHS.Sgf
import Khumba.GoHS.Test.Common
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

tests = testGroup "Khumba.GoHS.Sgf" [
  expandCoordListTests,
  emptyNodeTests,
  rootNodeTests,
  findPropertyTests,
  addPropertyTests,
  addChildTests,
  toSimpleTextTests,
  fromSimpleTextTests,
  cnotTests,
  colorToMoveTests,
  propertyMetadataTests
  ]

expandCoordListTests = testGroup "expandCoordList" [
  testCase "works with an empty CoordList" $
    [] @=? expandCoordList CoordList {coordListSingles = [], coordListRects = []},

  testCase "works for single points" $ do
    [(1,2)] @=? expandCoordList CoordList { coordListSingles = [(1,2)]
                                          , coordListRects = []
                                          }
    [(3,4), (1,2)] @=? expandCoordList CoordList { coordListSingles = [(3,4), (1,2)]
                                                 , coordListRects = []
                                                 }
    let ten = [(i,i) | i <- [1..10]]
    ten @=? expandCoordList CoordList { coordListSingles = ten
                                      , coordListRects = []
                                      },

  -- TODO Test that a 1x1 rectangle is rejected.

  testCase "works for a nx1 rect" $
    [(5,2), (5,3), (5,4)] @=? expandCoordList CoordList { coordListSingles = []
                                                        , coordListRects = [((5,2), (5,4))]
                                                        },

  testCase "works for a 1xn rect" $
    [(1,0), (1,1), (1,2), (1,3)] @=? expandCoordList CoordList { coordListSingles = []
                                                               , coordListRects = [((1,0), (1,3))]
                                                               },

  testCase "works for an mxn rect" $
    [(m,n) | m <- [2..5], n <- [3..7]] @=? expandCoordList CoordList { coordListSingles = []
                                                                     , coordListRects = [((2,3), (5,7))]
                                                                     },

  -- TODO Test that x0 > x1 || y0 > y1 is rejected.

  testCase "works with multiple rects" $
    [(0,0), (0,1), (0,2), (3,4), (4,4), (5,4)] @=?
    expandCoordList CoordList { coordListSingles = []
                              , coordListRects = [((0,0), (0,2)), ((3,4), (5,4))]
                              },

  testCase "concatenates single points and rects" $
    [(1,1), (0,0), (2,2), (2,3), (2,4)] @=?
    expandCoordList CoordList { coordListSingles = [(1,1), (0,0)]
                              , coordListRects = [((2,2), (2,4))]
                              }
  ]

emptyNodeTests = testGroup "emptyNode" [
  testCase "has no properties" $ [] @=? nodeProperties emptyNode,
  testCase "has no children" $ [] @=? nodeChildren emptyNode
  ]

rootNodeTests = testGroup "rootNode" [
  testCase "sets SZ correctly" $ do
    assertElem (SZ 9 9) $ nodeProperties $ rootNode 9 9
    assertElem (SZ 19 19) $ nodeProperties $ rootNode 19 19
    assertElem (SZ 9 5) $ nodeProperties $ rootNode 9 5
  ]

findPropertyTests = testGroup "findProperty" [
  testCase "returns Nothing for an empty node" $
    Nothing @=? findProperty (mk []) (const True),

  testCase "returns Nothing if no properties match" $
    Nothing @=? findProperty (mk [FF 4, GM 1, ST defaultVariationMode, SZ 9 9]) (const False),

  testCase "finds present properties" $ do
     Just (B (Just (2,3))) @=?
       findProperty (mk [B (Just (2,3))])
                    (\p -> case p of { B (Just (2,3)) -> True; _ -> False })
     Just (W Nothing) @=?
       findProperty (mk [B Nothing, W Nothing])
                    (\p -> case p of { W {} -> True; _ -> False })
     Just IT @=?
       findProperty (mk [IT, GW Double2])
                    (\p -> case p of { IT -> True; _ -> False })
     Just (GW Double2) @=?
       findProperty (mk [IT, GW Double2])
                    (\p -> case p of { IT -> False; _ -> True }),

  testCase "doesn't find absent properties" $ do
    Nothing @=? findProperty (mk [B Nothing])
                             (\p -> case p of { W Nothing -> True; _ -> False })
    Nothing @=? findProperty (mk [FF 4, GM 1, ST defaultVariationMode, SZ 9 9])
                             (\p -> case p of
                               FF {} -> False
                               GM {} -> False
                               ST {} -> False
                               SZ {} -> False
                               _ -> True)
  ]
  where mk properties = emptyNode { nodeProperties = properties }

addPropertyTests = testGroup "addProperty" [
  testCase "adds properties in order" $ do
    let prop1 = B (Just (6,6))
        prop2 = RE GameResultVoid
        prop3 = C "Game over."
        node1 = addProperty prop1 emptyNode
        node2 = addProperty prop2 node1
        node3 = addProperty prop3 node2
    node1 @?= emptyNode { nodeProperties = [prop1] }
    node2 @?= emptyNode { nodeProperties = [prop1, prop2] }
    node3 @?= emptyNode { nodeProperties = [prop1, prop2, prop3] }
  ]

addChildTests = testGroup "addChild" [
  testCase "adds children in order" $ do
    let node1 = emptyNode { nodeProperties = [B (Just (0,0))] }
        node2 = emptyNode { nodeProperties = [W (Just (1,1))] }
        node3 = emptyNode { nodeProperties = [B (Just (2,2))] }
        node4 = emptyNode { nodeProperties = [W Nothing] }
        node2' = addChild node3 node2
        node1' = addChild node2' node1
        node1'' = addChild node4 node1'
    node1'' @?= mk [B (Just (0,0))]
                   [mk [W (Just (1,1))]
                       [mk [B (Just (2,2))]
                           []],
                    mk [W Nothing]
                       []]
  ]
  where mk properties children = emptyNode { nodeProperties = properties
                                           , nodeChildren = children
                                           }

-- TODO Test validateNode.

toSimpleTextTests = testGroup "toSimpleText" [
  testCase "accepts the empty string" $ SimpleText "" @=? toSimpleText "",

  testCase "passes through strings without newlines" $ do
    SimpleText "Hello." @=? toSimpleText "Hello."
    SimpleText "Bad for B." @=? toSimpleText "Bad for B."
    SimpleText "[4k?]" @=? toSimpleText "[4k?]"
    SimpleText printableAsciiChars @=? toSimpleText printableAsciiChars,

  testCase "converts newlines to spaces" $ do
    SimpleText " " @=? toSimpleText "\n"
    SimpleText "Hello, world." @=? toSimpleText "Hello,\nworld."
    SimpleText "Hello,   world." @=? toSimpleText "Hello, \n world."
    SimpleText " Hello, world. " @=? toSimpleText "\nHello, world.\n"
  ]

fromSimpleTextTests = testGroup "fromSimpleText" [
  testCase "returns the string within a SimpleText" $ do
    "" @=? fromSimpleText (SimpleText "")
    "Hello, world." @=? fromSimpleText (SimpleText "Hello, world.")
    printableAsciiChars @=? fromSimpleText (SimpleText printableAsciiChars)
  ]

cnotTests = testGroup "cnot" [
  testCase "changes Black to White" $ White @=? cnot Black,
  testCase "changes White to Black" $ Black @=? cnot White
  ]

colorToMoveTests = testGroup "colorToMove" [
  testCase "creates Black moves" $ do
    B (Just (3,2)) @=? colorToMove Black (3,2)
    B (Just (0,0)) @=? colorToMove Black (0,0),

  testCase "creates White moves" $ do
    W (Just (18,18)) @=? colorToMove White (18,18)
    W (Just (15,16)) @=? colorToMove White (15,16)
  ]

propertyMetadataTests = testGroup "property metadata" [
  testCase "game info properties" $ gameInfoProperties @=? filterTo GameInfoProperty allProperties,
  testCase "general properties" $ generalProperties @=? filterTo GeneralProperty allProperties,
  testCase "move properties" $ moveProperties @=? filterTo MoveProperty allProperties,
  testCase "root properties" $ rootProperties @=? filterTo RootProperty allProperties,
  testCase "setup properties" $ setupProperties @=? filterTo SetupProperty allProperties,

  testCase "inherited properties" $ [DD cl] @=? filter propertyInherited allProperties
  ]
  where filterTo propType = filter ((propType ==) . propertyType)
        moveProperties = [-- Move properties.
                          B Nothing, KO, MN 1, W Nothing,
                          -- Move annotation properties.
                          BM db, DO, IT, TE db]
        setupProperties = [-- Setup properties.
                           AB cl, AE cl, AW cl, PL Black]
        generalProperties = [-- Node annotation properties.
                             C "", DM db, GB db, GW db, HO db, N st, UC db, V rv,
                             -- Markup properties.
                             AR [], CR cl, DD cl, LB [], LN [], MA cl, SL cl, SQ cl, TR cl,
                             -- Guess this fits here.
                             UnknownProperty "" ""]
        rootProperties = [-- Root properties.
                          AP st st, CA st, FF 1, GM 1, ST vm, SZ 1 1]
        gameInfoProperties = [-- Game info properties.
                              AN st, BR st, BT st, CP st, DT st, EV st, GC st, GN st, ON st, OT st,
                              PB st, PC st, PW st, RE GameResultVoid, RO st, RU RulesetJapanese,
                              SO st, TM rv, US st, WR st, WT st]
        allProperties = moveProperties ++ setupProperties ++ generalProperties ++
                        rootProperties ++ gameInfoProperties
        cl = emptyCoordList
        db = Double1
        st = toSimpleText ""
        rv = 1
        vm = defaultVariationMode
