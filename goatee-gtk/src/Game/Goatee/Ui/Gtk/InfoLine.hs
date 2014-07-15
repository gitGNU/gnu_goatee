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

-- | A text widget that displays information about the game, including some
-- overall information, as well as the current board position.
module Game.Goatee.Ui.Gtk.InfoLine (
  InfoLine,
  create,
  destroy,
  myLabel,
  ) where

import Data.Maybe (fromMaybe)
import Game.Goatee.Sgf.Board
import Game.Goatee.Sgf.Monad
import Game.Goatee.Ui.Gtk.Common
import Graphics.UI.Gtk (Label, labelNew, labelSetMarkup)

data InfoLine ui = InfoLine { myUi :: ui
                            , myRegistrations :: ViewRegistrations
                            , myLabel :: Label
                            }

instance UiCtrl ui => UiView (InfoLine ui) ui where
  viewName = const "InfoLine"
  viewCtrl = myUi
  viewRegistrations = myRegistrations

create :: UiCtrl ui => ui -> IO (InfoLine ui)
create ui = do
  label <- labelNew Nothing
  registrations <- viewNewRegistrations

  let me = InfoLine { myUi = ui
                    , myRegistrations = registrations
                    , myLabel = label
                    }

  initialize me
  return me

initialize :: UiCtrl ui => InfoLine ui -> IO ()
initialize me = do
  let updateAfter = afterGo . updateWithCursor me =<< getCursor
  viewRegister me childAddedEvent $ const $ const updateAfter
  viewRegister me navigationEvent $ const updateAfter
  viewRegister me propertiesModifiedEvent $ const $ const updateAfter
  update me

destroy :: UiCtrl ui => InfoLine ui -> IO ()
destroy = viewUnregisterAll

update :: UiCtrl ui => InfoLine ui -> IO ()
update me = do
  cursor <- readCursor $ myUi me
  updateWithCursor me cursor

updateWithCursor :: UiCtrl ui => InfoLine ui -> Cursor -> IO ()
updateWithCursor me cursor =
  labelSetMarkup (myLabel me) $ generateMarkup cursor

generateMarkup :: Cursor -> String
generateMarkup cursor =
  let board = cursorBoard cursor
      gameInfoMsg = fromMaybe "" $ do
        let info = boardGameInfo board
        black <- gameInfoBlackName info
        white <- gameInfoWhiteName info
        let renderRank = maybe "" (\x -> " (" ++ x ++ ")")
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
