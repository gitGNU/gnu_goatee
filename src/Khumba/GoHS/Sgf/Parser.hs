module Khumba.GoHS.Sgf.Parser ( ParseError
                              , parseString
                              , parseFile

                                -- * Visible for testing.
                              , single
                              , number
                              , real
                              , double
                              , color
                              , text
                              , simpleText
                              , line
                              , point
                              , stone
                              , move
                              , compose
                              ) where

import Control.Applicative ((<$), (<$>), (<*), (*>), (<*>))
import Control.Monad
import Data.Char
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Khumba.GoHS.Sgf
import Khumba.GoHS.Common
import Text.ParserCombinators.Parsec

parseString :: String -> Either String Collection
parseString str = case parse collection "<collection>" str of
  Left err -> Left $ show err
  Right (Collection roots) -> onLeft concatErrors $
                              onRight Collection $
                              andEithers $
                              map ttToPass roots
  where -- SGF allows B[tt] and W[tt] to represent passes on boards <=19x19.
        -- Convert any passes from this format to B[] and W[] in a root node and
        -- its descendents.
        ttToPass root = Right $
          let SZ width height = fromMaybe (SZ defaultSize defaultSize) $
                                findProperty root isSZ
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
        isSZ prop = case prop of
          SZ _ _ -> True
          _ -> False
        concatErrors errs = "The following errors occurred while parsing:" ++
                            concatMap ("\n-> " ++) errs

parseFile :: String -> IO (Either String Collection)
parseFile = fmap parseString . readFile

collection :: CharParser () Collection
collection = fmap Collection (spaces *> many (gameTree <* spaces) <* eof)
             <?> "collection"

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
property = do
  name <- many1 upper
  spaces *> fromMaybe (unknownProperty name) (Map.lookup name properties)

properties :: Map String (CharParser () Property)
properties = Map.fromList [
  ("B", single $ B <$> move),
  -- TODO Parse KO.  How to parse word boundaries?
  ("MN", single $ MN <$> number),
  ("W", single $ W <$> move),

  ("AB", AB <$> listOfPoint),
  ("AE", AE <$> listOfPoint),
  ("AW", AW <$> listOfPoint),
  ("PL", single $ PL <$> color),

  ("SZ", single $ (\x -> SZ x x) <$> number),  -- TODO Support [w:h].

  ("BR", single $ BR <$> simpleText False),
  ("PB", single $ PB <$> simpleText False),
  ("PW", single $ PW <$> simpleText False),
  ("WR", single $ WR <$> simpleText False),

  ("AR", AR <$> listOf (compose point point)),
  ("CR", CR <$> listOfPoint),
  ("DD", DD <$> listOfPoint),
  -- TODO Parse LB.
  -- TODO Parse LN.
  ("MA", MA <$> listOfPoint),
  ("SQ", SQ <$> listOfPoint),
  ("SL", SL <$> listOfPoint),
  ("TR", TR <$> listOfPoint),

  ("VW", VW <$> elistOfPoint)
  ]

unknownProperty :: String -> CharParser () Property
unknownProperty name = do
  value <- fmap (concatMap $ \x -> "[" ++ x ++ "]") $
           many (char '[' *> many (try (char '\\' *> anyChar) <|> noneOf "]") <* char ']')
  return $ UnknownProperty name value

propertyParser :: String -> CharParser a Property -> CharParser a Property
propertyParser name valueParser = string name *> spaces *> valueParser

single :: CharParser a b -> CharParser a b
single valueParser = char '[' *> valueParser <* char ']'

listOf :: CharParser a b -> CharParser a [b]
listOf valueParser = many1 (single valueParser <* spaces)
                     <?> "list"

listOfPoint :: CharParser () CoordList
listOfPoint = mconcat <$> listOf pointListEntry
  where pointListEntry = listR <$> try (compose point point)
                         <|> list1 <$> point
                         <?> "point list"
        list1 point = CoordList { coordListSingles = [point]
                                , coordListRects = []
                                }
        listR (from, to) = CoordList { coordListSingles = []
                                     , coordListRects = [(from, to)]
                                     }

elistOfPoint :: CharParser () CoordList
elistOfPoint = try listOfPoint
               <|> emptyCoordList <$ string "[]"
               <?> "point elist"

number :: (Num a, Read a) => CharParser () a
number = read . fst <$> number' <?> "number"

number' :: CharParser () (String, Bool)
number' = do
  sign <- option "" $ choice ["" <$ char '+',
                              "-" <$ char '-']
  digits <- many1 digit
  return (sign ++ digits, not $ null sign)

real :: CharParser () RealValue
real = real' <?> "real"

real' :: CharParser () RealValue
real' = do
  (whole, isNegative) <- number'
  let wholePart = toRational (read whole :: Integer)
  -- Try to read a fractional part of the number.
  -- If we fail, just return the whole part.
  option wholePart $ try $ do
    fractionalStr <- char '.' *> many1 digit
    let fractionalPart = toRational (read fractionalStr) / 10 ^ length fractionalStr
    return $ (if isNegative then (-) else (+)) wholePart fractionalPart

double :: CharParser () DoubleValue
double = choice [Double1 <$ char '1',
                 Double2 <$ char '2']
         <?> "double"

color :: CharParser () Color
color = choice [Black <$ char 'B',
                White <$ char 'W']
        <?> "color"

-- | A parser for SGF text property values.  Its argument should be true if the
-- text is inside of a composed property value, so ':' should terminate the
-- value in addition to ']'.
text :: Bool -> CharParser () Text
text isComposed = toText <$> textParser isComposed
                  <?> "text"

-- | A parser for SGF SimpleText property values.
simpleText :: Bool -> CharParser () SimpleText
simpleText isComposed = toSimpleText <$> textParser isComposed
                        <?> "simpleText"

textParser :: Bool -> CharParser () String
textParser isComposed =
  catMaybes <$> many textChar'
  where textChar' = textChar (if isComposed then ":]\\" else "]\\")

textChar :: String -> CharParser () (Maybe Char)
textChar specialChars = choice [Just <$> char '\n',
                                Just ' ' <$ space,
                                try (char '\\' *> (Nothing <$ char '\n'
                                                   <|> Just <$> anyChar)),
                                Just <$> noneOf specialChars]

line :: CharParser () Int
line = do
  c <- line'
  return $ if isUpper c
           then ord c - ord 'A' + 26
           else ord c - ord 'a'

line' :: CharParser () Char
line' = oneOf $ ['a'..'z'] ++ ['A'..'Z']

stone :: CharParser () Coord
stone = liftM2 (,) line line <?> "stone"

point :: CharParser () Coord
point = liftM2 (,) line line <?> "point"

move :: CharParser () (Maybe Coord)
move = try (liftM Just $ liftM2 (,) line line) <|> return Nothing

compose :: CharParser u a -> CharParser u b -> CharParser u (a, b)
compose first second = do
  x <- first
  char ':'
  y <- second
  return (x, y)
