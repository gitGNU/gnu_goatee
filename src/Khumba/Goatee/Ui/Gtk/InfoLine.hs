-- | A text widget that displays information about the game, including some
-- overall information, as well as the current board position.
module Khumba.Goatee.Ui.Gtk.InfoLine (
  InfoLine
  , create
  , initialize
  , myLabel
  ) where

import Data.IORef
import Data.Maybe
import Graphics.UI.Gtk hiding (Cursor)
import Khumba.Goatee.Sgf
import Khumba.Goatee.Sgf.Monad (getCursor, navigationEvent)
import Khumba.Goatee.Ui.Gtk.Common

data InfoLine ui = InfoLine { myUi :: UiRef ui
                            , myLabel :: Label
                            , myNavigationHandler :: IORef (Maybe Registration)
                            }

create :: UiCtrl ui => UiRef ui -> IO (InfoLine ui)
create uiRef = do
  label <- labelNew Nothing
  navigationHandler <- newIORef Nothing
  return InfoLine { myUi = uiRef
                  , myLabel = label
                  , myNavigationHandler = navigationHandler
                  }

initialize :: UiCtrl ui => InfoLine ui -> IO ()
initialize infoLine = do
  ui <- readUiRef $ myUi infoLine
  let updateWithCursor cursor = labelSetMarkup (myLabel infoLine) (generateMarkup cursor)
      onNavigate _ = afterGo . updateWithCursor =<< getCursor
  writeIORef (myNavigationHandler infoLine) . Just =<< register ui navigationEvent onNavigate
  updateWithCursor =<< readCursor ui

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
