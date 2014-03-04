-- | A text widget that displays information about the game, including some
-- overall information, as well as the current board position.
module Khumba.Goatee.Ui.Gtk.InfoLine (
  InfoLine
  , create
  , destruct
  , initialize
  , myLabel
  ) where

import Data.Maybe
import Graphics.UI.Gtk hiding (Cursor)
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Monad (getCursor, childAddedEvent, navigationEvent, propertiesChangedEvent)
import Khumba.Goatee.Ui.Gtk.Common

data InfoLine ui = InfoLine { myUi :: UiRef ui
                            , myRegistrations :: ViewRegistrations
                            , myLabel :: Label
                            }

instance UiCtrl ui => UiView (InfoLine ui) ui where
  viewName = const "InfoLine"
  viewUiRef = myUi
  viewRegistrations = myRegistrations

create :: UiCtrl ui => UiRef ui -> IO (InfoLine ui)
create uiRef = do
  label <- labelNew Nothing
  registrations <- viewNewRegistrations
  return InfoLine { myUi = uiRef
                  , myRegistrations = registrations
                  , myLabel = label
                  }

initialize :: UiCtrl ui => InfoLine ui -> IO ()
initialize me = do
  let updateAfter = afterGo . updateWithCursor me =<< getCursor
  viewRegister me childAddedEvent $ const $ const updateAfter
  viewRegister me navigationEvent $ const updateAfter
  viewRegister me propertiesChangedEvent $ const $ const updateAfter
  update me

destruct :: UiCtrl ui => InfoLine ui -> IO ()
destruct = viewUnregisterAll

update :: UiCtrl ui => InfoLine ui -> IO ()
update me = do
  ui <- readUiRef $ myUi me
  cursor <- readCursor ui
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
