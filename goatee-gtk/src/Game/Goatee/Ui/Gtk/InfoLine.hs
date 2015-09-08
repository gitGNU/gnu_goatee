-- This file is part of Goatee.
--
-- Copyright 2014-2015 Bryan Gardiner
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

{-# LANGUAGE CPP #-}

-- | A text widget that displays information about the game, including some
-- overall information, as well as the current board position.
module Game.Goatee.Ui.Gtk.InfoLine (
  InfoLine,
  create,
  destroy,
  myWidget,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Maybe (fromMaybe)
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Monad
import Game.Goatee.Lib.Types
import Game.Goatee.Ui.Gtk.Common
import Graphics.UI.Gtk (Label, Widget, labelNew, labelSetMarkup, toWidget)

data InfoLine ui = InfoLine
  { myUi :: ui
  , myState :: ViewState
  , myWidget :: Widget
  , myLabel :: Label
  }

instance UiCtrl go ui => UiView go ui (InfoLine ui) where
  viewName = const "InfoLine"
  viewCtrl = myUi
  viewState = myState
  viewUpdate = update

create :: UiCtrl go ui => ui -> IO (InfoLine ui)
create ui = do
  label <- labelNew (Nothing :: Maybe String)
  state <- viewStateNew

  let me = InfoLine { myUi = ui
                    , myState = state
                    , myWidget = toWidget label
                    , myLabel = label
                    }

  initialize me
  return me

initialize :: UiCtrl go ui => InfoLine ui -> IO ()
initialize me = do
  register me
    [ AnyEvent childAddedEvent
    , AnyEvent childDeletedEvent
    , AnyEvent navigationEvent
    , AnyEvent propertiesModifiedEvent
    ]
  viewUpdate me

destroy :: UiCtrl go ui => InfoLine ui -> IO ()
destroy = viewDestroy

update :: UiCtrl go ui => InfoLine ui -> IO ()
update me =
  labelSetMarkup (myLabel me) . generateMarkup =<< readCursor (myUi me)

generateMarkup :: Cursor -> String
generateMarkup cursor =
  let board = cursorBoard cursor
      gameInfoMsg = fromMaybe "" $ do
        let info = boardGameInfo board
        black <- sgfToString <$> gameInfoBlackName info
        white <- sgfToString <$> gameInfoWhiteName info
        let renderRank = maybe "" (\x -> " (" ++ sgfToString x ++ ")")
            blackRank = renderRank $ gameInfoBlackRank info
            whiteRank = renderRank $ gameInfoWhiteRank info
        return $ white ++ whiteRank ++ " vs. " ++ black ++ blackRank ++ "\n"
      siblingMsg = case cursorParent cursor of
        Nothing -> "Start of game."
        Just parent ->
          let parentChildCount = cursorChildCount parent
          in if parentChildCount > 1
             then "Variation " ++ show (cursorChildIndex cursor + 1) ++
                  "/" ++ show parentChildCount ++ "."
             else ""
      childrenMsg = let childCount = cursorChildCount cursor
                    in case childCount of
                      0 -> "End of variation."
                      1 -> ""
                      _ -> "<b>" ++ show childCount ++ " variations from here.</b>"
  in gameInfoMsg ++ "Move " ++ show (boardMoveNumber board) ++ ", " ++
     show (boardPlayerTurn board) ++ " to play.  Captures: B+" ++
     show (boardBlackCaptures board) ++ ", W+" ++ show (boardWhiteCaptures board)
     ++ ".\n" ++ siblingMsg ++
     (if siblingMsg /= [] && childrenMsg /= [] then "  " else "") ++ childrenMsg
