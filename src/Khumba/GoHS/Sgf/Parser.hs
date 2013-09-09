module Khumba.GoHS.Sgf.Parser (ParseError, parseString, parseFile) where

import Control.Applicative ((<$), (<*), (*>), (<*>))
import Control.Monad
import Data.Char
import Khumba.GoHS.Sgf
import Text.ParserCombinators.Parsec

parseString :: String -> Either ParseError [Node]
parseString = parse sgf "<sgf>"

parseFile :: String -> IO (Either ParseError [Node])
parseFile = fmap (parse sgf "<sgf>") . readFile

sgf :: CharParser () [Node]
sgf = spaces *> many (gameTree <* spaces) <* eof
      <?> "sgf"

gameTree :: CharParser () Node
gameTree = do
  char '('
  nodes <- spaces *> many1 (node <* spaces) <?> "sequence"
  subtrees <- many (gameTree <* spaces) <?> "subtrees"
  char ')'
  let (sequence, [final]) = splitAt (length nodes - 1) nodes
  return $ foldr (\seqNode childNode -> seqNode { nodeChildren = [childNode] })
                 (final { nodeChildren = subtrees })
                 sequence

node :: CharParser () Node
node = fmap (\props -> emptyNode { nodeProperties = props })
       (char ';' *> spaces *> many (property <* spaces)
        <?> "node")

property :: CharParser () Property
property = choice [try $ parseProperty "B" $ fmap B move,
                   try $ parseProperty "W" $ fmap W move,
                   try $ parseProperty "SZ" $ fmap (\x -> SZ x x) number,
                   unknownProperty]

unknownProperty :: CharParser () Property
unknownProperty = do
  name <- many1 upper
  value <- fmap (concatMap $ \x -> "[" ++ x ++ "]") $
           many (char '[' *> many (try escapedChar <|> noneOf "]") <* char ']')
  return $ UnknownProperty name value

escapedChar :: CharParser () Char
escapedChar = char '\\' *> anyChar

parseProperty :: String -> CharParser a Property -> CharParser a Property
parseProperty name valueParser = string name *> asValue valueParser

asValue :: CharParser a Property -> CharParser a Property
asValue valueParser = char '[' *> valueParser <* char ']'

number :: CharParser () Int
number = fmap read number' <?> "number"

number' :: CharParser () String
number' = do
  sign <- option "" $ choice ["" <$ char '+',
                              "-" <$ char '-']
  digits <- many1 digit
  return $ sign ++ digits

real :: CharParser () RealValue
real = fmap read real' <?> "real"

real' :: CharParser () String
real' = do
  wholePart <- number'
  -- Try to read a fractional part of the number.
  -- If we fail, just return the whole part.
  option wholePart $ do
    char '.'
    fractionalPart <- many1 digit
    return $ wholePart ++ "." ++ fractionalPart

double :: CharParser () DoubleValue
double = choice [Double1 <$ char '1',
                 Double2 <$ char '2']
         <?> "double"

color :: CharParser () Color
color = choice [Black <$ char 'B',
                White <$ char 'W']
        <?> "color"

-- TODO simpleText

-- TODO text

line :: CharParser () Int
line = do
  c <- line'
  return $ if isUpper c
           then ord c - ord 'A' + 26
           else ord c - ord 'a'

line' :: CharParser () Char
line' = oneOf $ ['a'..'z'] ++ ['A'..'Z']

point :: CharParser () Coord
point = liftM2 (,) line line <?> "point"

stone :: CharParser () Coord
stone = liftM2 (,) line line <?> "stone"

move :: CharParser () Coord
move = liftM2 (,) line line <?> "move"

-- TODO compose
