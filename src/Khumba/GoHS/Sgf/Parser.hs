module Khumba.GoHS.Sgf.Parser (ParseError, parseString, parseFile) where

import Control.Applicative ((<$), (<*), (*>), (<*>))
import Control.Monad
import Data.Char
import Khumba.GoHS.Sgf
import Khumba.GoHS.Common
import Text.ParserCombinators.Parsec

parseString :: String -> Either String [Node]
parseString str = case parse sgf "<sgf>" str of
  Left err -> Left $ show err
  Right roots -> onLeft concatErrors $ andEithers $ map ttToPass roots
  where -- SGF allows B[tt] and W[tt] to represent passes on boards <=19x19.
        -- Convert any passes from this format to B[] and W[] in a root node and
        -- its descendents.
        ttToPass root = case findProperty root isSZ of
          Nothing ->
            Left $ "Missing size property (SZ) in root node: " ++ show root
          Just (SZ width height) ->
            Right $ if width <= 19 && height <= 19
                    then root
                    else ttToPass' width height root
        -- Convert a node and its descendents.
        ttToPass' width height node =
          node { nodeProperties = map ttToPass'' $ nodeProperties node
               , nodeChildren = map (ttToPass' width height) $ nodeChildren node
               }
        -- Convert a property.
        ttToPass'' prop = case prop of
          B (Just (20, 20)) -> B Nothing
          W (Just (20, 20)) -> W Nothing
          _ -> prop
        isSZ prop = case prop of
          SZ _ _ -> True
          _ -> False
        concatErrors errs = "The following errors occurred while parsing:" ++
                            concatMap ("\n-> " ++) errs

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

move :: CharParser () (Maybe Coord)
move = try (liftM Just $ liftM2 (,) line line) <|> return Nothing

-- TODO compose
