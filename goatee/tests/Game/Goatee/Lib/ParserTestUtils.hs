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

module Game.Goatee.Lib.ParserTestUtils (
  parseOrFail,
  parseAndFail,
  assertParse,
  assertNoParse,
  ) where

import Control.Applicative ((<*))
import Game.Goatee.Lib.Parser
import Game.Goatee.Lib.Tree
import Test.HUnit (assertFailure)
import Text.ParserCombinators.Parsec (Parser, eof, parse)

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

-- Parses a string using the given parser and expects the parser to consume the
-- entire input.  On success, executes the continuation function with the
-- result.  Otherwise, causes an assertion failure.
assertParse :: Parser a -> String -> (a -> IO ()) -> IO ()
assertParse parser input cont = case parse (parser <* eof) "<assertParse>" input of
  Left error -> assertFailure $ "Failed to parse: " ++ show error
  Right result -> cont result

-- Tries to parse a string using the given parser.  If the parse succeeds then
-- this function causes an assertion failure, otherwise this function succeeds.
assertNoParse :: Show a => Parser a -> String -> IO ()
assertNoParse parser input = case parse (parser <* eof) "<assertNoParse>" input of
  Left _ -> return ()
  Right result -> assertFailure $
                  "Expected " ++ show input ++ " not to parse.  " ++
                  "Parsed to " ++ show result ++ "."
