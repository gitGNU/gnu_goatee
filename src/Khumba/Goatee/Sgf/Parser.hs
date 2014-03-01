-- | A parser for reading SGF files.
module Khumba.Goatee.Sgf.Parser (
  ParseError
  , parseString
  , parseFile

    -- * Visible for testing
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
  , gameType
  , variationMode
  , boardSize
  , gameResult
  , ruleset
  ) where

import Control.Applicative ((<$), (<$>), (<*), (*>))
import Control.Monad
import Data.Char
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.Tree
import Khumba.Goatee.Sgf.Types
import Khumba.Goatee.Common
import Text.ParserCombinators.Parsec

-- TODO Support FF versions 1-4.
supportedFormatVersions :: [Int]
supportedFormatVersions = [4]

-- | The default SGF version to use when @FF[]@ is not specified in a root node.
--
-- This value is actually INCORRECT: SGF defines it to be 1, but because we
-- don't support version 1 yet, for the sake of ignoring this issue (for now!)
-- in tests, we fix the default to be 4.
--
-- TODO Fix the default version to be 1 as SGF mandates.
defaultFormatVersion :: Int
defaultFormatVersion = 4

supportedGameTypes :: [Int]
supportedGameTypes = [1 {- Go -}]

-- | The maximum board size allowed by FF[4].
maxBoardSize :: Int
maxBoardSize = 52

parseString :: String -> Either String Collection
parseString str = case parse collection "<collection>" str of
  Left err -> Left $ show err
  Right (Collection roots) -> onLeft concatErrors $
                              onRight Collection $
                              andEithers $
                              map processRoot roots
  where processRoot :: Node -> Either String Node
        processRoot = checkFormatVersion . ttToPass

        -- Ensures that we are parsing an SGF version that we understand.
        -- TODO Try to proceed, if it makes sense.
        checkFormatVersion :: Node -> Either String Node
        checkFormatVersion root =
          let version = case findProperty root isFF of
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

        isFF prop = case prop of { FF {} -> True; _ -> False }
        isSZ prop = case prop of { SZ {} -> True; _ -> False }
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
  ("KO", return KO),
  ("MN", single $ MN <$> number),
  ("W", single $ W <$> move),

  ("AB", AB <$> listOfPoint),
  ("AE", AE <$> listOfPoint),
  ("AW", AW <$> listOfPoint),
  ("PL", single $ PL <$> color),

  ("C", single $ C <$> text False),
  ("DM", single $ DM <$> double),
  ("GB", single $ GB <$> double),
  ("GW", single $ GW <$> double),
  ("HO", single $ HO <$> double),
  ("N", single $ N <$> simpleText False),
  ("UC", single $ UC <$> double),
  ("V", single $ V <$> real),

  ("BM", single $ BM <$> double),
  ("DO", return DO),
  ("IT", return IT),
  ("TE", single $ TE <$> double),

  ("AR", AR <$> listOf (compose point point)),
  ("CR", CR <$> listOfPoint),
  ("DD", DD <$> listOfPoint),
  ("LB", LB <$> listOf (compose point $ simpleText True)),
  ("LN", LN <$> listOf (compose point point)),
  ("MA", MA <$> listOfPoint),
  ("SQ", SQ <$> listOfPoint),
  ("SL", SL <$> listOfPoint),
  ("TR", TR <$> listOfPoint),

  ("AP", single $ uncurry AP <$> compose (simpleText True) (simpleText True)),
  ("CA", single $ CA <$> simpleText False),
  ("FF", single $ FF <$> number),
  ("GM", single $ GM <$> gameType),
  ("ST", single $ ST <$> variationMode),
  ("SZ", single boardSize),

  ("AN", single $ AN <$> simpleText False),
  ("BR", single $ BR <$> simpleText False),
  ("BT", single $ BT <$> simpleText False),
  ("CP", single $ CP <$> simpleText False),
  ("DT", single $ DT <$> simpleText False),
  ("EV", single $ EV <$> simpleText False),
  ("GC", single $ GC <$> simpleText False),
  ("GN", single $ GN <$> simpleText False),
  ("ON", single $ ON <$> simpleText False),
  ("OT", single $ OT <$> simpleText False),
  ("PB", single $ PB <$> simpleText False),
  ("PC", single $ PC <$> simpleText False),
  ("PW", single $ PW <$> simpleText False),
  ("RE", single $ RE <$> gameResult),
  ("RO", single $ RO <$> simpleText False),
  ("RU", single $ RU <$> ruleset),
  ("SO", single $ SO <$> simpleText False),
  ("TM", single $ TM <$> real),
  ("US", single $ US <$> simpleText False),
  ("WR", single $ WR <$> simpleText False),
  ("WT", single $ WT <$> simpleText False),

  ("VW", VW <$> elistOfPoint)
  ]

unknownProperty :: String -> CharParser () Property
unknownProperty name = do
  value <- concatMap (\x -> "[" ++ x ++ "]") <$>
           many (char '[' *> many (try (char '\\' *> anyChar) <|> noneOf "]") <* char ']')
  return $ UnknownProperty name value

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
-- text is inside of a composed property value, so @\':\'@ should terminate the
-- value in addition to @']'@.
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

gameType :: CharParser () Int
gameType = do
  game <- number
  if game `elem` supportedGameTypes
    then return game
    else fail $ "Unsupported game type " ++ show game ++ ".  Only " ++
         show supportedGameTypes ++ " are supported."

variationMode :: CharParser () VariationMode
variationMode = do
  value <- number
  case toVariationMode value of
    Just mode -> return mode
    Nothing -> fail $ "Invalid variation mode " ++ show value ++ "."

boardSize :: CharParser () Property
boardSize = do
  size@(SZ w h) <- try (do (w, h) <- compose number number
                           if w /= h
                             then return $ SZ w h
                             else fail $
                                  show w ++ "x" ++ show h ++ " square board dimensions " ++
                                  "must be specified with a single number.")
                   <|> (\w -> SZ w w) <$> number
                   <?> "boardSize"
  if w < 1 || h < 1 || w > maxBoardSize || h > maxBoardSize
    then fail $ "Invalid board size " ++ show w ++ "x" ++ show h ++ "."
    else return size

gameResult :: CharParser () GameResult
gameResult = GameResultDraw <$ try (string "0")
             <|> GameResultDraw <$ try (string "Draw")
             <|> GameResultVoid <$ try (string "Void")
             <|> GameResultUnknown <$ try (string "?")
             <|> (do player <- color
                     char '+'
                     reason <- winReason
                     return $ GameResultWin player reason)
             <?> "gameResult"
  where winReason = WinByScore <$> try real
                    <|> WinByResignation <$ try (string "Resign")
                    <|> WinByResignation <$ try (string "R")
                    <|> WinByTime <$ try (string "Time")
                    <|> WinByTime <$ try (string "T")
                    <|> WinByForfeit <$ try (string "Forfeit")
                    <|> WinByForfeit <$ try (string "F")

ruleset :: CharParser () Ruleset
ruleset = toRuleset . fromSimpleText <$> simpleText False
