-----------------------------------------------------------------------------
--
-- Module      :  Model
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Model (

) where



data Game = Game { gameInfo :: GameInfo
                 , gameRoot :: GameTree
                 } deriving (Show)

data GameInfo = GameInfo { gameBlackName :: String
                         , gameBlackRank :: Rational
                         , gameWhiteName :: String
                         , gameWhiteRank :: Rational
                         , gameKomi :: Rational
                         , gameHandicap :: [Position]
                         } deriving (Show)

data GameTree = GameTree { treeNode :: GameNode
                         , treeComment :: String
                         , treeChildren :: [GameTree]
                         } deriving (Show)

data GameNode = MoveNode { movePosition :: Position
                         }
              | EditNode {
