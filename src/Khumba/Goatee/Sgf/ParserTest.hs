module Khumba.Goatee.Sgf.ParserTest (tests) where

import Control.Applicative ((<$>), (<*))
import Control.Monad
import Data.Maybe
import Khumba.Goatee.Common
import Khumba.Goatee.Sgf.Parser
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.TestUtils
import Khumba.Goatee.Sgf.Tree
import Khumba.Goatee.Sgf.Types
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Node, Test)
import Text.ParserCombinators.Parsec (CharParser, eof, parse)

-- Parses a string as a complete SGF document.  On success, executes the
-- continuation function with the result.  Otherwise, causes an assertion
-- failure.
parseOrFail :: String -> (Node -> IO ()) -> IO ()
parseOrFail input cont = case parseString input of
  Left error -> assertFailure $ "Failed to parse SGF: " ++ error
  Right (Collection roots) -> case roots of
    root:[] -> cont root
    _ -> assertFailure $ "Expected a single root node, got: " ++ show roots

-- Parses a string as a complete SGF document and expects failure.
parseAndFail :: String -> IO ()
parseAndFail input = case parseString input of
  Left _ -> return ()
  Right result -> assertFailure $ "Expected " ++ show input ++
                  " not to parse.  Parsed to " ++ show result ++ "."

-- Parses a string using the given parser.  On success, executes the
-- continuation function with the result.  Otherwise, causes an assertion
-- failure.
assertParse :: CharParser () a -> String -> (a -> IO ()) -> IO ()
assertParse parser input cont = case parse (parser <* eof) "<assertParse>" input of
  Left error -> assertFailure $ "Failed to parse: " ++ show error
  Right result -> cont result

-- Tries to parse a string using the given parser.  If the parse succeeds then
-- this function causes an assertion failure, otherwise this function succeeds.
assertNoParse :: Show a => CharParser () a -> String -> IO ()
assertNoParse parser input = case parse (parser <* eof) "<assertNoParse>" input of
  Left _ -> return ()
  Right result -> assertFailure $
                  "Expected " ++ show input ++ " not to parse.  " ++
                  "Parsed to " ++ show result ++ "."

tests = testGroup "Khumba.Goatee.Sgf.Parser" [
  baseCaseTests,
  whitespaceTests,
  passConversionTests,
  propertyValueArityTests,
  propertyValueTests,
  movePropertyTests,
  setupPropertyTests,
  nodeAnnotationPropertyTests,
  moveAnnotationPropertyTests,
  rootPropertyTests
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
                          node [W $ Just (3,3)]]),

  testCase "parses with spaces between nodes" $
    parseOrFail "(;SZ[1] ;B[])" (@?= rootNode 1 1 [] [node [B Nothing]]),

  testCase "parses with spaces between properties" $
    parseOrFail "(;SZ[1] AB[aa])" (@?= rootNode 1 1 [AB $ coords [(0,0)]] []),

  testCase "parses with spaces between a property's name and value" $
    parseOrFail "(;SZ [1])" (@?= rootNode 1 1 [] []),

  testCase "parses with spaces between property values" $
    parseOrFail "(;SZ[2]AB[aa] [bb])" (@?= rootNode 2 2 [AB $ coords [(0,0), (1,1)]] []),

  testCase "parses with spaces between many elements" $
    parseOrFail " ( ; SZ [4] ; AB [aa:ad] [bb] AW [cc] ; W [dd] ; B [] ) "
    (@?= rootNode 4 4 [] [node1 [AB $ coords' [(1,1)] [((0,0), (0,3))],
                                 AW $ coords [(2,2)]] $
                          node1 [W $ Just (3,3)] $
                          node [B Nothing]])

  -- TODO Test handling of whitespace between an unknown property name and
  -- [value].
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

  testCase "doesn't convert non-move properties" $ do
    -- TODO These should error, rather than parsing fine.
    parseOrFail "(;SZ[9];AB[tt])" (@?= rootNode 9 9 [] [node [AB $ coords [(19, 19)]]])
    parseOrFail "(;SZ[9];TR[tt])" (@?= rootNode 9 9 [] [node [TR $ coords [(19, 19)]]])
  ]

propertyValueArityTests = testGroup "property value arities" [
  testGroup "single values (single)" [
    testCase "accepts a property that requires a single number" $
      parseOrFail "(;SZ[1])" (@?= node [SZ 1 1]),

    testCase "accepts a property that requires a single point" $
      parseOrFail "(;B[dd])" (@?= node [B $ Just (3, 3)])
    ],

  testGroup "lists (listOf)" [
    testCase "doesn't accept an empty list" $
      parseAndFail "(;AR[])",

    testCase "accepts a single value" $
      parseOrFail "(;AR[aa:bb])" (@?= node [AR [((0, 0), (1, 1))]]),

    testCase "accepts two values" $
      parseOrFail "(;AR[aa:bb][cc:de])" (@?= node [AR [((0, 0), (1, 1)),
                                                       ((2, 2), (3, 4))]])
    ],

  testGroup "point lists (listOfPoint)" [
    testCase "doesn't accept an empty list" $
      parseAndFail "(;AB[])",

    testCase "accepts a single point" $
      parseOrFail "(;AB[aa])" (@?= node [AB $ coords [(0, 0)]]),

    testCase "accepts two points" $
      parseOrFail "(;AB[aa][bb])" (@?= node [AB $ coords [(0, 0), (1, 1)]]),

    testCase "accepts a rectangle" $
      parseOrFail "(;AB[aa:bb])" (@?= node [AB $ coords' [] [((0, 0), (1, 1))]]),

    testCase "accepts two rectangles" $
      parseOrFail "(;AB[aa:bb][cd:de])" (@?= node [AB $ coords' [] [((0, 0), (1, 1)),
                                                                    ((2, 3), (3, 4))]])
    ],

  testGroup "point elists (elistOfPoint)" [
    testCase "accepts an empty list" $
      parseOrFail "(;VW[])" (@?= node [VW $ coords []]),

    testCase "accepts single points" $
      parseOrFail "(;VW[aa][bb])" (@?= node [VW $ coords [(0, 0), (1, 1)]]),

    testCase "accepts a rectangle" $
      parseOrFail "(;VW[aa:bb])" (@?= node [VW $ coords' [] [((0, 0), (1, 1))]]),

    testCase "accepts two rectangles" $
      parseOrFail "(;VW[aa:bb][cc:dd])" (@?= node [VW $ coords' [] [((0, 0), (1, 1)),
                                                                    ((2, 2), (3, 3))]])
    ]

  -- TODO Test that invalid rectangles such as cd:dc fail to parse (or rather,
  -- get corrected with a warning).
  ]

propertyValueTests = testGroup "property values" [
  testGroup "number" $ integerTestsFor number,

  testGroup "real" $ integerTestsFor real ++ [
    testCase "parses a decimal zero" $
      assertParse real "0.0" (@?= 0),

    testCase "parses fractional positive numbers" $ do
      assertParse real "0.5" (@?= (1/2))
      assertParse real "0.00125" (@?= (1/800))
      assertParse real "3.14" (@?= (314/100))
      assertParse real "10.0" (@?= 10),

    testCase "parses fractional negative numbers" $ do
      assertParse real "-0.5" (@?= (-1/2))
      assertParse real "-0.00125" (@?= (-1/800))
      assertParse real "-3.14" (@?= (-314/100))
      assertParse real "-10.0" (@?= (-10))
    ],

  testGroup "double" [
    testCase "parses 1" $ assertParse double "1" (@?= Double1),
    testCase "parses 2" $ assertParse double "2" (@?= Double2)
    ],

  testGroup "color" [
    testCase "parses B" $ assertParse color "B" (@?= Black),
    testCase "parses W" $ assertParse color "W" (@?= White)
    ],

  testGroup "text" $ textTestsFor text fromText False ++
    textUnescapedNewlinePreservingTests (single $ fromText <$> text False),

  testGroup "text composed" $ textTestsFor text fromText True ++
    textUnescapedNewlinePreservingTests (single $ fromText <$> text True),

  testGroup "simpleText" $ textTestsFor simpleText fromSimpleText False ++
    textUnescapedNewlineConvertingTests (single $ fromSimpleText <$> simpleText False),

  testGroup "simpleText composed" $ textTestsFor simpleText fromSimpleText True ++
    textUnescapedNewlineConvertingTests (single $ fromSimpleText <$> simpleText True),

  testGroup "line" [
    testCase "parses all valid values" $ do
      let cases = zip (['a'..'z'] ++ ['A'..'Z']) [0..]
      forM_ cases $ \(char, expected) ->
        assertParse line [char] (@?= expected)
    ],

  testGroup "stone" $ pointTestsFor stone id,

  testGroup "point" $ pointTestsFor point id,

  testGroup "move" $ pointTestsFor move Just ++ [
    testCase "parses an empty move as a pass" $
      assertParse move "" (@?= Nothing),

    testCase "doesn't parse a partial move" $
      assertNoParse (single move) "[a]"
    ],

  testGroup "compose" [
    testCase "parses number pairs" $
      assertParse (compose number number) "0:-5" (@?= (0, -5)),

    testCase "parses point pairs" $
      assertParse (compose point point) "aa:bb" (@?= ((0, 0), (1, 1)))
    ],

  testGroup "gameType" [
    testCase "succeeds for the Go game type" $
      assertParse gameType "1" (@?= 1),

    testCase "fails for non-Go game types" $
      forM_ (0:[2..20]) $ \x ->
        assertNoParse gameType $ show x
    ],

  testGroup "variationMode" [
    testCase "mode 0" $
      assertParse variationMode "0" (@?= VariationMode ShowChildVariations True),

    testCase "mode 1" $
      assertParse variationMode "1" (@?= VariationMode ShowCurrentVariations True),

    testCase "mode 2" $
      assertParse variationMode "2" (@?= VariationMode ShowChildVariations False),

    testCase "mode 3" $
      assertParse variationMode "3" (@?= VariationMode ShowCurrentVariations False)
    ],

  testGroup "boardSize" [
    testCase "parses square boards" $ do
      assertParse boardSize "1" (@?= SZ 1 1)
      assertParse boardSize "4" (@?= SZ 4 4)
      assertParse boardSize "9" (@?= SZ 9 9)
      assertParse boardSize "19" (@?= SZ 19 19)
      assertParse boardSize "52" (@?= SZ 52 52),

    testCase "parses rectangular boards" $ do
      assertParse boardSize "1:2" (@?= SZ 1 2)
      assertParse boardSize "9:5" (@?= SZ 9 5)
      assertParse boardSize "19:9" (@?= SZ 19 9),

    testCase "rejects boards of non-positive size" $ do
      assertNoParse boardSize "0"
      assertNoParse boardSize "-1"
      assertNoParse boardSize "0:5"
      assertNoParse boardSize "5:0"
      assertNoParse boardSize "0:-2",

    testCase "rejects square boards given in rectangular format" $ do
      assertNoParse boardSize "1:1"
      assertNoParse boardSize "13:13"
    ],

  testGroup "gameResult" [
    testCase "draw" $ do
      assertParse gameResult "0" (@?= GameResultDraw)
      assertParse gameResult "Draw" (@?= GameResultDraw),

    testCase "void" $
      assertParse gameResult "Void" (@?= GameResultVoid),

    testCase "unknown" $
      assertParse gameResult "?" (@?= GameResultUnknown),

    testCase "Black wins by points" $ do
      assertParse gameResult "B+0.5" (@?= GameResultWin Black (WinByScore 0.5))
      assertParse gameResult "B+11" (@?= GameResultWin Black (WinByScore 11))
      assertParse gameResult "B+354.5" (@?= GameResultWin Black (WinByScore 354.5)),

    testCase "Black wins by resignation" $ do
      assertParse gameResult "B+R" (@?= GameResultWin Black WinByResignation)
      assertParse gameResult "B+Resign" (@?= GameResultWin Black WinByResignation),

    testCase "Black wins on time" $ do
      assertParse gameResult "B+T" (@?= GameResultWin Black WinByTime)
      assertParse gameResult "B+Time" (@?= GameResultWin Black WinByTime),

    testCase "Black wins by forfeit" $ do
      assertParse gameResult "B+F" (@?= GameResultWin Black WinByForfeit)
      assertParse gameResult "B+Forfeit" (@?= GameResultWin Black WinByForfeit),

    testCase "White wins by points" $ do
      assertParse gameResult "W+0.5" (@?= GameResultWin White (WinByScore 0.5))
      assertParse gameResult "W+11" (@?= GameResultWin White (WinByScore 11))
      assertParse gameResult "W+354.5" (@?= GameResultWin White (WinByScore 354.5)),

    testCase "White wins by resignation" $ do
      assertParse gameResult "W+R" (@?= GameResultWin White WinByResignation)
      assertParse gameResult "W+Resign" (@?= GameResultWin White WinByResignation),

    testCase "White wins on time" $ do
      assertParse gameResult "W+T" (@?= GameResultWin White WinByTime)
      assertParse gameResult "W+Time" (@?= GameResultWin White WinByTime),

    testCase "White wins by forfeit" $ do
      assertParse gameResult "W+F" (@?= GameResultWin White WinByForfeit)
      assertParse gameResult "W+Forfeit" (@?= GameResultWin White WinByForfeit)
    ],

  testGroup "ruleset" [
    testCase "parses known rules" $ do
      assertParse ruleset "AGA" (@?= KnownRuleset RulesetAga)
      assertParse ruleset "Goe" (@?= KnownRuleset RulesetIng)
      assertParse ruleset "Japanese" (@?= KnownRuleset RulesetJapanese)
      assertParse ruleset "NZ" (@?= KnownRuleset RulesetNewZealand),

    testCase "parses unknown rules" $ do
      assertParse ruleset "Foo" (@?= UnknownRuleset "Foo")
      assertParse ruleset "First capture" (@?= UnknownRuleset "First capture")
    ]
  ]

  where integerTestsFor parser = [
          testCase "parses 0" $ do
            assertParse parser "0" (@?= 0)
            assertParse parser "+0" (@?= 0)
            assertParse parser "-0" (@?= 0),

          testCase "parses positive integers" $ do
            assertParse parser "1" (@?= 1)
            assertParse parser "20" (@?= 20)
            assertParse parser "4294967296" (@?= (2 ^ 32))
            assertParse parser "18446744073709551616" (@?= (2 ^ 64)),

          testCase "parses positive integers with the plus sign" $ do
            assertParse parser "+1" (@?= 1)
            assertParse parser "+20" (@?= 20)
            assertParse parser "+4294967296" (@?= (2 ^ 32))
            assertParse parser "+18446744073709551616" (@?= (2 ^ 64)),

          testCase "parses negative integers" $ do
            assertParse parser "-1" (@?= (-1))
            assertParse parser "-20" (@?= (-20))
            assertParse parser "-4294967296" (@?= (- (2 ^ 32)))
            assertParse parser "-18446744073709551616" (@?= (- (2 ^ 64)))
          ]

        textTestsFor makeParser toString testComposed =
          let rawParser = makeParser testComposed
              parser = single $ toString <$> rawParser
              composedParser = single $ mapTuple toString <$> compose rawParser rawParser
          in catMaybes [
            Just $ testCase "parses an empty string" $
              assertParse parser "[]" (@?= ""),

            Just $ testCase "parses a short string" $
              assertParse parser "[Hello, world.]" (@?= "Hello, world."),

            Just $ testCase "preserves leading and trailing whitespace" $
              assertParse parser "[ \tHi. \t]" (@?= "  Hi.  "),

            Just $ testCase "parses escaped backslashes" $ do
              assertParse parser "[\\\\]" (@?= "\\")
              assertParse parser "[\\\\\\\\]" (@?= "\\\\")
              assertNoParse parser "[\\]"
              assertNoParse parser "[\\\\\\]",

            Just $ testCase "parses escaped ']'s" $ do
              assertParse parser "[\\]]" (@?= "]")
              assertParse parser "[\\]\\\\\\]]" (@?= "]\\]"),

            Just $ if testComposed
                   then testCase "parses escaped ':'s" $ do
                     assertParse parser "[\\:]" (@?= ":")
                     assertParse parser "[\\:\\\\\\:]" (@?= ":\\:")
                     assertNoParse parser "[:]"
                   else testCase "parses unescaped ':'s" $ do
                     assertParse parser "[:]" (@?= ":")
                     assertParse parser "[::]" (@?= "::")
                     -- An escaped colon should parse just the same.
                     assertParse parser "[\\:]" (@?= ":"),

            if not testComposed
              then Nothing
              else Just $ testCase "supports composed values" $ do
                assertParse composedParser "[:]" (@?= ("", ""))
                assertParse composedParser "[a:]" (@?= ("a", ""))
                assertParse composedParser "[:z]" (@?= ("", "z"))
                assertParse composedParser "[a:z]" (@?= ("a", "z"))
                assertParse composedParser "[a\\\\:z]" (@?= ("a\\", "z"))
                assertParse composedParser "[a\\:b:y\\:z]" (@?= ("a:b", "y:z"))
                assertNoParse composedParser "[]",

            -- Tests non-newline whitespace replacement.  Newline handling is
            -- specific to individual parsers.
            Just $ testCase "replaces whitespace with spaces" $
              assertParse parser "[\t\r\f\v]" (@?= "    "),

            Just $ testCase "removes escaped newlines" $ do
              assertParse parser "[\\\n]" (@?= "")
              assertParse parser "[foo\\\nbar]" (@?= "foobar")
              assertParse parser "[foo \\\n bar]" (@?= "foo  bar")
            ]

        textUnescapedNewlinePreservingTests parser = [
          testCase "preserves unescaped newlines" $ do
            assertParse parser "[\n]" (@?= "\n")
            assertParse parser "[\n\n]" (@?= "\n\n")
            assertParse parser "[foo\nbar]" (@?= "foo\nbar")
            assertParse parser "[foo \n bar]" (@?= "foo \n bar")
          ]

        textUnescapedNewlineConvertingTests parser = [
          testCase "converts unescaped newlines to spaces" $ do
            assertParse parser "[\n]" (@?= " ")
            assertParse parser "[\n\n]" (@?= "  ")
            assertParse parser "[foo\nbar]" (@?= "foo bar")
            assertParse parser "[foo \n bar]" (@?= "foo   bar")
          ]

        pointTestsFor parser f = [
          testCase "parses boundary points" $ do
            assertParse parser "aa" (@?= f (0, 0))
            assertParse parser "zz" (@?= f (25, 25))
            assertParse parser "AA" (@?= f (26, 26))
            assertParse parser "ZZ" (@?= f (51, 51)),

          testCase "parses coordinate order correctly" $ do
            assertParse parser "ab" (@?= f (0, 1))
            assertParse parser "ba" (@?= f (1, 0))
          ]

movePropertyTests = testGroup "move properties" [
  testGroup "B" [
    testCase "parses moves" $ do
      parseOrFail "(;B[aa])" (@?= node [B $ Just (0, 0)])
      parseOrFail "(;B[cp])" (@?= node [B $ Just (2, 15)]),
    testCase "parses passes" $
      parseOrFail "(;B[])" (@?= node [B Nothing])
    ],
  testCase "KO parses" $
    parseOrFail "(;KO)" (@?= node [KO]),
  -- TODO Test MN (assert positive).
  testGroup "W" [
    testCase "parses moves" $ do
      parseOrFail "(;W[aa])" (@?= node [W $ Just (0, 0)])
      parseOrFail "(;W[cp])" (@?= node [W $ Just (2, 15)]),
    testCase "parses passes" $
      parseOrFail "(;W[])" (@?= node [W Nothing])
    ]
  ]

setupPropertyTests = testGroup "setup properties" [
  testCase "AB parses" $ do
    parseOrFail "(;AB[ab])" (@?= node [AB $ coords [(0, 1)]])
    parseOrFail "(;AB[ab][cd:ef])" (@?= node [AB $ coords' [(0, 1)] [((2, 3), (4, 5))]]),
  testCase "AE parses" $ do
    parseOrFail "(;AE[ae])" (@?= node [AE $ coords [(0, 4)]])
    parseOrFail "(;AE[ae][ff:gg])" (@?= node [AE $ coords' [(0, 4)] [((5, 5), (6, 6))]]),
  testCase "AW parses" $ do
    parseOrFail "(;AW[aw])" (@?= node [AW $ coords [(0, 22)]])
    parseOrFail "(;AW[aw][xy:yz])" (@?= node [AW $ coords' [(0, 22)] [((23, 24), (24, 25))]]),
  testCase "PL parses" $ do
    parseOrFail "(;PL[B])" (@?= node [PL Black])
    parseOrFail "(;PL[W])" (@?= node [PL White])
  ]

nodeAnnotationPropertyTests = testGroup "node annotation properties" [
  testCase "C parses" $
    parseOrFail "(;C[Me [30k\\]: What is White doing??\\\n\n:(])"
      (@?= node [C $ toText "Me [30k]: What is White doing??\n:("]),
  testCase "DM parses" $ do
    parseOrFail "(;DM[1])" (@?= node [DM Double1])
    parseOrFail "(;DM[2])" (@?= node [DM Double2]),
  testCase "GB parses" $ do
    parseOrFail "(;GB[1])" (@?= node [GB Double1])
    parseOrFail "(;GB[2])" (@?= node [GB Double2]),
  testCase "GW parses" $ do
    parseOrFail "(;GW[1])" (@?= node [GW Double1])
    parseOrFail "(;GW[2])" (@?= node [GW Double2]),
  testCase "HO parses" $ do
    parseOrFail "(;HO[1])" (@?= node [HO Double1])
    parseOrFail "(;HO[2])" (@?= node [HO Double2]),
  testCase "N parses" $
    parseOrFail "(;N[The best\\\n\nmove])" (@?= node [N $ toSimpleText "The best move"]),
  testCase "UC parses" $ do
    parseOrFail "(;UC[1])" (@?= node [UC Double1])
    parseOrFail "(;UC[2])" (@?= node [UC Double2]),
  testCase "V parses" $ do
    parseOrFail "(;V[-34.5])" (@?= node [V (-34.5)])
    parseOrFail "(;V[50])" (@?= node [V 50])
  ]

moveAnnotationPropertyTests = testGroup "move annotation properties" [
  testCase "BM parses" $ do
    parseOrFail "(;BM[1])" (@?= node [BM Double1])
    parseOrFail "(;BM[2])" (@?= node [BM Double2]),
  testCase "DO parses" $
    parseOrFail "(;DO)" (@?= node [DO]),
  testCase "IT parses" $
    parseOrFail "(;IT)" (@?= node [IT]),
  testCase "TE parses" $ do
    parseOrFail "(;TE[1])" (@?= node [TE Double1])
    parseOrFail "(;TE[2])" (@?= node [TE Double2])
  ]

-- TODO Test markup properties.

rootPropertyTests = testGroup "root properties" [
  testCase "AP parses" $
    parseOrFail "(;AP[GoGoGo:1.2.3])" (@?= node [AP (toSimpleText "GoGoGo") (toSimpleText "1.2.3")]),
  testCase "CA parses" $ do
    parseOrFail "(;CA[UTF-8])" (@?= node [CA $ toSimpleText "UTF-8"])
    parseOrFail "(;CA[ISO-8859-1])" (@?= node [CA $ toSimpleText "ISO-8859-1"]),
  testGroup "FF" [
    testCase "accepts version 4" $
      parseOrFail "(;FF[4])" (@?= node [FF 4]),
    testCase "rejects versions 1-3" $ do
      parseAndFail "(;FF[1])"
      parseAndFail "(;FF[2])"
      parseAndFail "(;FF[3])"
    ],
  testGroup "GM" [
    testCase "parses 1 (Go)" $
      parseOrFail "(;GM[1])" (@?= node [GM 1]),
    testCase "rejects unsupported games" $
      forM_ [2..16] $ \x -> parseAndFail $ "(;GM[" ++ show x ++ "]"
    ],
  testGroup "ST" [
    testCase "parses valid variation modes" $ do
      parseOrFail "(;ST[0])" (@?= node [ST $ VariationMode ShowChildVariations True])
      parseOrFail "(;ST[1])" (@?= node [ST $ VariationMode ShowCurrentVariations True])
      parseOrFail "(;ST[2])" (@?= node [ST $ VariationMode ShowChildVariations False])
      parseOrFail "(;ST[3])" (@?= node [ST $ VariationMode ShowCurrentVariations False]),
    testCase "rejects invalid variation modes" $
      forM_ [4..10] $ \x -> parseAndFail $ "(;ST[" ++ show x ++ "]"
    ],
  testGroup "SZ" [
    testCase "parses square boards" $
      forM_ [1..52] $ \x ->
        parseOrFail ("(;SZ[" ++ show x ++ "])") (@?= node [SZ x x]),
    testCase "parses nonsquare boards" $ do
      parseOrFail "(;SZ[1:2])" (@?= node [SZ 1 2])
      parseOrFail "(;SZ[1:9])" (@?= node [SZ 1 9])
      parseOrFail "(;SZ[1:19])" (@?= node [SZ 1 19])
      parseOrFail "(;SZ[2:1])" (@?= node [SZ 2 1])
      parseOrFail "(;SZ[9:1])" (@?= node [SZ 9 1])
      parseOrFail "(;SZ[19:1])" (@?= node [SZ 19 1])
      parseOrFail "(;SZ[19:52])" (@?= node [SZ 19 52])
      parseOrFail "(;SZ[10:16])" (@?= node [SZ 10 16]),
    testCase "rejects invalid sizes" $ do
      -- Boards must have length at least 1...
      parseAndFail "(;SZ[0])"
      parseAndFail "(;SZ[-1])"
      -- ...and at most 52.
      parseAndFail "(;SZ[53])"
      parseAndFail "(;SZ[54])"
      parseAndFail "(;SZ[0:19])"
      parseAndFail "(;SZ[19:0])"
      parseAndFail "(;SZ[9:53])"
      parseAndFail "(;SZ[53:9])",
    -- This is specified by the SGF spec:
    testCase "rejects square boards defined with two numbers" $ do
      parseAndFail "(;SZ[1:1])"
      parseAndFail "(;SZ[19:19])"
    ]
  ]

-- TODO Test the test of the properties.
