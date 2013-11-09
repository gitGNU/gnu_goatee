module Khumba.GoHS.Sgf.ParserTest (tests) where

import Khumba.GoHS.Sgf
import Khumba.GoHS.SgfTestUtils
import Khumba.GoHS.Sgf.Parser
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Node, Test)

parseOrFail :: String -> (Node -> IO ()) -> IO ()
parseOrFail input cont = case parseString input of
  Left error -> assertFailure $ "Failed to parse SGF: " ++ error
  Right (Collection roots) -> case roots of
    root:[] -> cont root
    _ -> assertFailure $ "Expected a single root node, got: " ++ show roots

tests = testGroup "Khumba.GoHS.Sgf.Parser" [
  baseCaseTests,
  whitespaceTests,
  passConversionTests
  ]

baseCaseTests = testGroup "base cases" [
  testCase "works with the trivial collection" $
    parseOrFail "(;)" (@?= emptyNode),

  testCase "works with only a size property" $ do
    parseOrFail "(;SZ[1])" (@?= rootNode 1 1 [] [])
    parseOrFail "(;SZ[9])" (@?= rootNode 9 9 [] [])
  ]

whitespaceTests = testGroup "whitespace handling" [
  testCase "parses with no extra whitespace" $
    parseOrFail "(;SZ[4];AB[aa][bb]AW[cc];W[dd])"
    (@?= rootNode 4 4 [] [node1 [AB $ coords [(0,0), (1,1)], AW $ coords [(2,2)]] $
                          node [W $ Just (3,3)]])
  ]

passConversionTests = testGroup "B[tt]/W[tt] pass conversion" [
  testCase "converts B[tt] for a board sizes <=19x19" $ do
    parseOrFail "(;SZ[1];B[tt])" (@?= rootNode 1 1 [] [node [B Nothing]])
    parseOrFail "(;SZ[9];B[tt])" (@?= rootNode 9 9 [] [node [B Nothing]])
    parseOrFail "(;SZ[19];B[tt])" (@?= rootNode 19 19 [] [node [B Nothing]]),

  testCase "converts W[tt] for a board sizes <=19x19" $ do
    parseOrFail "(;SZ[1];W[tt])" (@?= rootNode 1 1 [] [node [W Nothing]])
    parseOrFail "(;SZ[9];W[tt])" (@?= rootNode 9 9 [] [node [W Nothing]])
    parseOrFail "(;SZ[19];W[tt])" (@?= rootNode 19 19 [] [node [W Nothing]]),

  testCase "doesn't convert B[tt] for a board sizes >19x19" $ do
    parseOrFail "(;SZ[20];B[tt])" (@?= rootNode 20 20 [] [node [B $ Just (19, 19)]])
    parseOrFail "(;SZ[21];B[tt])" (@?= rootNode 21 21 [] [node [B $ Just (19, 19)]]),

  testCase "doesn't convert W[tt] for a board sizes >19x19" $ do
    parseOrFail "(;SZ[20];W[tt])" (@?= rootNode 20 20 [] [node [W $ Just (19, 19)]])
    parseOrFail "(;SZ[21];W[tt])" (@?= rootNode 21 21 [] [node [W $ Just (19, 19)]]),

  testCase "doesn't convert non-move properties" $
    -- TODO These should error, rather than parsing fine.
    parseOrFail "(;SZ[9];AB[tt])" (@?= rootNode 9 9 [] [node [AB $ coords [(19, 19)]]])
  ]
