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

module Game.Goatee.Sgf.Property.ParserTest (tests) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import Game.Goatee.Common
import Game.Goatee.Sgf.ParserTestUtils
import Game.Goatee.Sgf.Property
import Game.Goatee.Sgf.TestInstances ()
import Game.Goatee.Sgf.TestUtils
import Game.Goatee.Sgf.Types
import Test.HUnit ((~:), (@?=), Test (TestList))
import Text.ParserCombinators.Parsec (Parser)

tests = "Game.Goatee.Sgf.Property.ParserTest" ~: TestList [
  -- Tests for public parsers.
  parserColorTests,
  parserDoubleTests,
  parserGameResultTests,
  parserIntegralTests,
  parserMoveTests,
  parserRealTests,
  parserRulesetTests,
  parserSizeTests,
  parserVariationModeTests,
  -- Tests for private parsers.
  lineTests,
  simpleTextTests,
  simpleTextComposedTests,
  textTests,
  textComposedTests,
  -- Miscellaneous tests.
  propertyValueArityTests
  ]

parserColorTests = "colorParser" ~: TestList [
  "parses B" ~: assertParse colorParser "[B]" (@?= Black),
  "parses W" ~: assertParse colorParser "[W]" (@?= White)
  ]

parserDoubleTests = "doubleParser" ~: TestList [
  "parses 1" ~: assertParse doubleParser "[1]" (@?= Double1),
  "parses 2" ~: assertParse doubleParser "[2]" (@?= Double2)
  ]

parserGameResultTests = "gameResultParser" ~: TestList [
  "draw" ~: do
    assertParse gameResultParser "[0]" (@?= GameResultDraw)
    assertParse gameResultParser "[Draw]" (@?= GameResultDraw),

  "void" ~:
    assertParse gameResultParser "[Void]" (@?= GameResultVoid),

  "unknown" ~:
    assertParse gameResultParser "[?]" (@?= GameResultUnknown),

  "Black wins by points" ~: do
    assertParse gameResultParser "[B+0.5]" (@?= GameResultWin Black (WinByScore $ read "0.5"))
    assertParse gameResultParser "[B+11]" (@?= GameResultWin Black (WinByScore $ read "11"))
    assertParse gameResultParser "[B+354.5]" (@?= GameResultWin Black (WinByScore $ read "354.5")),

  "Black wins by resignation" ~: do
    assertParse gameResultParser "[B+R]" (@?= GameResultWin Black WinByResignation)
    assertParse gameResultParser "[B+Resign]" (@?= GameResultWin Black WinByResignation),

  "Black wins on time" ~: do
    assertParse gameResultParser "[B+T]" (@?= GameResultWin Black WinByTime)
    assertParse gameResultParser "[B+Time]" (@?= GameResultWin Black WinByTime),

  "Black wins by forfeit" ~: do
    assertParse gameResultParser "[B+F]" (@?= GameResultWin Black WinByForfeit)
    assertParse gameResultParser "[B+Forfeit]" (@?= GameResultWin Black WinByForfeit),

  "White wins by points" ~: do
    assertParse gameResultParser "[W+0.5]" (@?= GameResultWin White (WinByScore $ read "0.5"))
    assertParse gameResultParser "[W+11]" (@?= GameResultWin White (WinByScore $ read "11"))
    assertParse gameResultParser "[W+354.5]" (@?= GameResultWin White (WinByScore $ read "354.5")),

  "White wins by resignation" ~: do
    assertParse gameResultParser "[W+R]" (@?= GameResultWin White WinByResignation)
    assertParse gameResultParser "[W+Resign]" (@?= GameResultWin White WinByResignation),

  "White wins on time" ~: do
    assertParse gameResultParser "[W+T]" (@?= GameResultWin White WinByTime)
    assertParse gameResultParser "[W+Time]" (@?= GameResultWin White WinByTime),

  "White wins by forfeit" ~: do
    assertParse gameResultParser "[W+F]" (@?= GameResultWin White WinByForfeit)
    assertParse gameResultParser "[W+Forfeit]" (@?= GameResultWin White WinByForfeit),

  "custom game results" ~: do
    assertParseToOther ""
    assertParseToOther "Everyone wins."
    assertParseToOther "W+Nuclear tesuji"
    assertParseToOther "B+Flamingo"
  ]
  where assertParseToOther input =
          assertParse gameResultParser ('[' : input ++ "]")
          (@?= GameResultOther (toSimpleText input))

parserIntegralTests = "integralParser" ~: TestList $ integerTestsFor integralParser

parserMoveTests = "moveParser" ~: TestList $ coordTestsFor moveParser Just ++ [
  "parses an empty move as a pass" ~: assertParse moveParser "[]" (@?= Nothing)
  ]

parserRealTests = "realParser" ~: TestList $ integerTestsFor realParser ++ [
  "parses a decimal zero" ~: assertParse realParser "[0.0]" (@?= 0),

  "parses fractional positive numbers" ~: do
    assertParse realParser "[0.5]" (@?= read "0.5")
    assertParse realParser "[00.001250]" (@?= read "0.00125")
    assertParse realParser "[3.14]" (@?= read "3.14")
    assertParse realParser "[10.0]" (@?= read "10"),

  "parses fractional negative numbers" ~: do
    assertParse realParser "[-0.5]" (@?= read "-0.5")
    assertParse realParser "[-00.001250]" (@?= read "-0.00125")
    assertParse realParser "[-3.14]" (@?= read "-3.14")
    assertParse realParser "[-10.0]" (@?= read "-10")
  ]

parserRulesetTests = "rulesetParser" ~: TestList [
  "parses known rules" ~: do
    assertParse rulesetParser "[AGA]" (@?= KnownRuleset RulesetAga)
    assertParse rulesetParser "[Goe]" (@?= KnownRuleset RulesetIng)
    assertParse rulesetParser "[Japanese]" (@?= KnownRuleset RulesetJapanese)
    assertParse rulesetParser "[NZ]" (@?= KnownRuleset RulesetNewZealand),

  "parses unknown rules" ~: do
    assertParse rulesetParser "[Foo]" (@?= UnknownRuleset "Foo")
    assertParse rulesetParser "[First capture]" (@?= UnknownRuleset "First capture")
  ]

parserSizeTests = "size" ~: TestList [
  "parses square boards" ~: do
    assertParse sizeParser "[1]" (@?= (1, 1))
    assertParse sizeParser "[4]" (@?= (4, 4))
    assertParse sizeParser "[9]" (@?= (9, 9))
    assertParse sizeParser "[19]" (@?= (19, 19))
    assertParse sizeParser "[52]" (@?= (52, 52)),

  "parses rectangular boards" ~: do
    assertParse sizeParser "[1:2]" (@?= (1, 2))
    assertParse sizeParser "[9:5]" (@?= (9, 5))
    assertParse sizeParser "[19:9]" (@?= (19, 9)),

  "rejects boards of non-positive size" ~: do
    assertNoParse sizeParser "[0]"
    assertNoParse sizeParser "[-1]"
    assertNoParse sizeParser "[0:5]"
    assertNoParse sizeParser "[5:0]"
    assertNoParse sizeParser "[0:-2]",

  "rejects square boards given in rectangular format" ~: do
    assertNoParse sizeParser "[1:1]"
    assertNoParse sizeParser "[13:13]"
  ]

parserVariationModeTests = "variationModeParser" ~: TestList [
  "mode 0" ~:
    assertParse variationModeParser "[0]" (@?= VariationMode ShowChildVariations True),

  "mode 1" ~:
    assertParse variationModeParser "[1]" (@?= VariationMode ShowCurrentVariations True),

  "mode 2" ~:
    assertParse variationModeParser "[2]" (@?= VariationMode ShowChildVariations False),

  "mode 3" ~:
    assertParse variationModeParser "[3]" (@?= VariationMode ShowCurrentVariations False)
  ]

lineTests = "line" ~: TestList [
  "parses all valid values" ~: do
     let cases = zip (['a'..'z'] ++ ['A'..'Z']) [0..]
     forM_ cases $ \(char, expected) ->
       assertParse line [char] (@?= expected)
  ]

simpleTextTests = "simpleText" ~: TestList $
  textTestsFor simpleText fromSimpleText False ++
  textUnescapedNewlineConvertingTests (fromSimpleText <$> simpleText False)

simpleTextComposedTests = "simpleText composed" ~: TestList $
  textTestsFor simpleText fromSimpleText True ++
  textUnescapedNewlineConvertingTests (fromSimpleText <$> simpleText True)

textTests = "text" ~: TestList $
  textTestsFor text id False ++
  textUnescapedNewlinePreservingTests (text False)

textComposedTests = "text composed" ~: TestList $
  textTestsFor text id True ++
  textUnescapedNewlinePreservingTests (text True)

propertyValueArityTests = "property value arities" ~: TestList [
  "single values (single)" ~: TestList [
    "accepts a property that requires a single number" ~:
      parseOrFail "(;SZ[1])" (@?= node [SZ 1 1]),

    "accepts a property that requires a single point" ~:
      parseOrFail "(;B[dd])" (@?= node [B $ Just (3, 3)])
    ],

  "lists (listOf)" ~: TestList [
    "doesn't accept an empty list" ~:
      parseAndFail "(;AR[])",

    "accepts a single value" ~:
      parseOrFail "(;AR[aa:bb])" (@?= node [AR [((0, 0), (1, 1))]]),

    "accepts two values" ~:
      parseOrFail "(;AR[aa:bb][cc:de])" (@?= node [AR [((0, 0), (1, 1)),
                                                       ((2, 2), (3, 4))]])
    ],

  "point lists (listOfPoint)" ~: TestList [
    "doesn't accept an empty list" ~:
      parseAndFail "(;AB[])",

    "accepts a single point" ~:
      parseOrFail "(;AB[aa])" (@?= node [AB $ coords [(0, 0)]]),

    "accepts two points" ~:
      parseOrFail "(;AB[aa][bb])" (@?= node [AB $ coords [(0, 0), (1, 1)]]),

    "accepts a rectangle" ~:
      parseOrFail "(;AB[aa:bb])" (@?= node [AB $ coords' [] [((0, 0), (1, 1))]]),

    "accepts two rectangles" ~:
      parseOrFail "(;AB[aa:bb][cd:de])" (@?= node [AB $ coords' [] [((0, 0), (1, 1)),
                                                                    ((2, 3), (3, 4))]])
    ],

  "point elists (elistOfPoint)" ~: TestList [
    "accepts an empty list" ~:
      parseOrFail "(;VW[])" (@?= node [VW $ coords []]),

    "accepts single points" ~:
      parseOrFail "(;VW[aa][bb])" (@?= node [VW $ coords [(0, 0), (1, 1)]]),

    "accepts a rectangle" ~:
      parseOrFail "(;VW[aa:bb])" (@?= node [VW $ coords' [] [((0, 0), (1, 1))]]),

    "accepts two rectangles" ~:
      parseOrFail "(;VW[aa:bb][cc:dd])" (@?= node [VW $ coords' [] [((0, 0), (1, 1)),
                                                                    ((2, 2), (3, 3))]])
    ]

  -- TODO Test that invalid rectangles such as cd:dc fail to parse (or rather,
  -- get corrected with a warning).
  ]

integerTestsFor :: (Eq a, Num a, Show a) => Parser a -> [Test]
integerTestsFor parser = [
  "parses 0" ~: do
    assertParse parser "[0]" (@?= 0)
    assertParse parser "[+0]" (@?= 0)
    assertParse parser "[-0]" (@?= 0),

  "parses positive integers" ~: do
    assertParse parser "[1]" (@?= 1)
    assertParse parser "[20]" (@?= 20)
    assertParse parser "[4294967296]" (@?= (2 ^ 32))
    assertParse parser "[18446744073709551616]" (@?= (2 ^ 64)),

  "parses positive integers with the plus sign" ~: do
    assertParse parser "[+1]" (@?= 1)
    assertParse parser "[+20]" (@?= 20)
    assertParse parser "[+4294967296]" (@?= (2 ^ 32))
    assertParse parser "[+18446744073709551616]" (@?= (2 ^ 64)),

  "parses negative integers" ~: do
    assertParse parser "[-1]" (@?= (-1))
    assertParse parser "[-20]" (@?= (-20))
    assertParse parser "[-4294967296]" (@?= (- (2 ^ 32)))
    assertParse parser "[-18446744073709551616]" (@?= (- (2 ^ 64)))
  ]

coordTestsFor :: (Eq a, Show a) => Parser a -> (Coord -> a) -> [Test]
coordTestsFor parser f = [
  "parses boundary points" ~: do
    assertParse parser "[aa]" (@?= f (0, 0))
    assertParse parser "[zz]" (@?= f (25, 25))
    assertParse parser "[AA]" (@?= f (26, 26))
    assertParse parser "[ZZ]" (@?= f (51, 51)),

  "parses coordinate order correctly" ~: do
    assertParse parser "[ab]" (@?= f (0, 1))
    assertParse parser "[ba]" (@?= f (1, 0)),

  "doesn't parse a partial point" ~:
    assertNoParse parser "[a]"
  ]

textTestsFor :: (Bool -> Parser a) -> (a -> String) -> Bool -> [Test]
textTestsFor makeParser toString testComposed =
  let rawParser = makeParser testComposed
      parser = toString <$> rawParser
      composedParser = mapTuple toString <$> compose rawParser rawParser
  in catMaybes [
    Just $ "parses an empty string" ~:
      assertParse parser "" (@?= ""),

    Just $ "parses a short string" ~:
      assertParse parser "Hello, world." (@?= "Hello, world."),

    Just $ "preserves leading and trailing whitespace" ~:
      assertParse parser " \tHi. \t" (@?= "  Hi.  "),

    Just $ "parses escaped backslashes" ~: do
      assertParse parser "\\\\" (@?= "\\")
      assertParse parser "\\\\\\\\" (@?= "\\\\")
      assertNoParse parser "\\"
      assertNoParse parser "\\\\\\",

    Just $ "parses escaped ']'s" ~: do
      assertParse parser "\\]" (@?= "]")
      assertParse parser "\\]\\\\\\]" (@?= "]\\]"),

    Just $ if testComposed
           then "parses escaped ':'s" ~: do
             assertParse parser "\\:" (@?= ":")
             assertParse parser "\\:\\\\\\:" (@?= ":\\:")
             assertNoParse parser ":"
           else "parses unescaped ':'s" ~: do
             assertParse parser ":" (@?= ":")
             assertParse parser "::" (@?= "::")
             -- An escaped colon should parse just the same.
             assertParse parser "\\:" (@?= ":"),

    if not testComposed
      then Nothing
      else Just $ "supports composed values" ~: do
        assertParse composedParser ":" (@?= ("", ""))
        assertParse composedParser "a:" (@?= ("a", ""))
        assertParse composedParser ":z" (@?= ("", "z"))
        assertParse composedParser "a:z" (@?= ("a", "z"))
        assertParse composedParser "a\\\\:z" (@?= ("a\\", "z"))
        assertParse composedParser "a\\:b:y\\:z" (@?= ("a:b", "y:z"))
        assertNoParse composedParser "",

    -- Tests non-newline whitespace replacement.  Newline handling is
    -- specific to individual parsers.
    Just $ "replaces whitespace with spaces" ~:
      assertParse parser "\t\r\f\v" (@?= "    "),

    Just $ "removes escaped newlines" ~: do
      assertParse parser "\\\n" (@?= "")
      assertParse parser "foo\\\nbar" (@?= "foobar")
      assertParse parser "foo \\\n bar" (@?= "foo  bar")
    ]

textUnescapedNewlinePreservingTests :: Parser String -> [Test]
textUnescapedNewlinePreservingTests parser = [
  "preserves unescaped newlines" ~: do
    assertParse parser "\n" (@?= "\n")
    assertParse parser "\n\n" (@?= "\n\n")
    assertParse parser "foo\nbar" (@?= "foo\nbar")
    assertParse parser "foo \n bar" (@?= "foo \n bar")
  ]

textUnescapedNewlineConvertingTests :: Parser String -> [Test]
textUnescapedNewlineConvertingTests parser = [
  "converts unescaped newlines to spaces" ~: do
    assertParse parser "\n" (@?= " ")
    assertParse parser "\n\n" (@?= "  ")
    assertParse parser "foo\nbar" (@?= "foo bar")
    assertParse parser "foo \n bar" (@?= "foo   bar")
  ]
