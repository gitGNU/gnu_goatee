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

-- | A parser for reading SGF files.
module Game.Goatee.Sgf.Parser (
  parseString,
  parseFile,
  propertyParser,
  ) where

import Control.Arrow ((+++))
import Control.Applicative ((<*), (*>))
import Data.Maybe (fromMaybe)
import Game.Goatee.Common
import Game.Goatee.Sgf.Property
import Game.Goatee.Sgf.Tree
import Game.Goatee.Sgf.Types
import Text.ParserCombinators.Parsec (
  (<?>), Parser, char, eof, many, many1, parse, spaces, upper,
  )

-- | Parses a string in SGF format.  Returns an error string if parsing fails.
parseString :: String -> Either String Collection
parseString str = case parse collectionParser "<collection>" str of
  Left err -> Left $ show err
  Right (Collection roots) -> (concatErrors +++ Collection) $
                              andEithers $
                              map processRoot roots
  where processRoot :: Node -> Either String Node
        processRoot = checkFormatVersion . ttToPass

        -- Ensures that we are parsing an SGF version that we understand.
        -- TODO Try to proceed, if it makes sense.
        checkFormatVersion :: Node -> Either String Node
        checkFormatVersion root =
          let version = case findProperty propertyFF root of
                Nothing -> defaultFormatVersion
                Just (FF x) -> x
                x -> error $ "Expected FF or nothing, received " ++ show x ++ "."
          in if version `elem` supportedFormatVersions
             then Right root
             else Left $
                  "Unsupported SGF version " ++ show version ++ ".  Only versions " ++
                  show supportedFormatVersions ++ " are supported."

        -- SGF allows B[tt] and W[tt] to represent passes on boards <=19x19.
        -- Convert any passes from this format to B[] and W[] in a root node and
        -- its descendents.
        ttToPass :: Node -> Node
        ttToPass root =
          let SZ width height = fromMaybe (SZ boardSizeDefault boardSizeDefault) $
                                findProperty propertySZ root
          in if width <= 19 && height <= 19
             then ttToPass' width height root
             else root

        -- Convert a node and its descendents.
        ttToPass' width height node =
          node { nodeProperties = map ttToPass'' $ nodeProperties node
               , nodeChildren = map (ttToPass' width height) $ nodeChildren node
               }

        -- Convert a property.
        ttToPass'' prop = case prop of
          B (Just (19, 19)) -> B Nothing
          W (Just (19, 19)) -> W Nothing
          _ -> prop

        concatErrors errs = "The following errors occurred while parsing:" ++
                            concatMap ("\n-> " ++) errs

-- | Parses a file in SGF format.  Returns an error string if parsing fails.
parseFile :: String -> IO (Either String Collection)
parseFile = fmap parseString . readFile

collectionParser :: Parser Collection
collectionParser =
  fmap Collection (spaces *> many (gameTreeParser <* spaces) <* eof) <?>
  "collection"

gameTreeParser :: Parser Node
gameTreeParser = do
  char '('
  nodes <- spaces *> many1 (nodeParser <* spaces) <?> "sequence"
  subtrees <- many (gameTreeParser <* spaces) <?> "subtrees"
  char ')'
  let (sequence, [final]) = splitAt (length nodes - 1) nodes
  return $ foldr (\seqNode childNode -> seqNode { nodeChildren = [childNode] })
                 (final { nodeChildren = subtrees })
                 sequence

nodeParser :: Parser Node
nodeParser =
  fmap (\props -> emptyNode { nodeProperties = props })
  (char ';' *> spaces *> many (propertyParser <* spaces) <?>
   "node")

propertyParser :: Parser Property
propertyParser = do
  name <- many1 upper
  spaces
  propertyValueParser $ descriptorForName name
