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
  findPropertyTests
  ]

expandCoordListTests = testGroup "expandCoordList" [
  testCase "should work with an empty CoordList" $
    [] @=? expandCoordList CoordList {coordListSingles = [], coordListRects = []},

  testCase "should work for single points" $ do
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

  testCase "should work for a nx1 rect" $
    [(5,2), (5,3), (5,4)] @=? expandCoordList CoordList { coordListSingles = []
                                                        , coordListRects = [((5,2), (5,4))]
                                                        },

  testCase "should work for a 1xn rect" $
    [(1,0), (1,1), (1,2), (1,3)] @=? expandCoordList CoordList { coordListSingles = []
                                                               , coordListRects = [((1,0), (1,3))]
                                                               },

  testCase "should work for an mxn rect" $
    [(m,n) | m <- [2..5], n <- [3..7]] @=? expandCoordList CoordList { coordListSingles = []
                                                                     , coordListRects = [((2,3), (5,7))]
                                                                     },

  -- TODO Test that x0 > x1 || y0 > y1 is rejected.

  testCase "should work with multiple rects" $
    [(0,0), (0,1), (0,2), (3,4), (4,4), (5,4)] @=?
    expandCoordList CoordList { coordListSingles = []
                              , coordListRects = [((0,0), (0,2)), ((3,4), (5,4))]
                              },

  testCase "should concatenate single points and rects" $
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
  testCase "should return Nothing for an empty node" $
    Nothing @=? findProperty (mk []) (const True),

  testCase "should find present properties" $ do
     Just (B (Just (2,3))) @=? findProperty (mk [B (Just (2,3))])
       (\p -> case p of { B (Just (2,3)) -> True; _ -> False })
     -- TODO More cases.
  ,

  testCase "should not find absent properties" $ do
    Nothing @=? findProperty (mk [B Nothing])
      (\p -> case p of { W Nothing -> True; _ -> False })
    -- TODO More cases.
  ]
  where mk properties = emptyNode { nodeProperties = properties }
