module Khumba.GoHS.Ui.Gtk.MainWindow ( MainWindow
                                     , create
                                     , display
                                     , myWindow
                                     ) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Tree (drawTree, unfoldTree)
import Graphics.UI.Gtk
import Khumba.GoHS.Sgf
import Khumba.GoHS.Ui.Gtk.Common
import qualified Khumba.GoHS.Ui.Gtk.Goban as Goban
import Khumba.GoHS.Ui.Gtk.Goban (Goban)
import qualified Khumba.GoHS.Ui.Gtk.InfoLine as InfoLine
import Khumba.GoHS.Ui.Gtk.InfoLine (InfoLine)

-- | If false, then the up and down keys will move toward and away
-- from the game tree root, and left and right will move between
-- siblings.  If true, these are reversed.
useHorizontalKeyNavigation :: Bool
useHorizontalKeyNavigation = True

keyNavActions :: UiCtrl a => Map String (a -> IO Bool)
keyNavActions = Map.fromList $
                if useHorizontalKeyNavigation
                then [("Up", goLeft),
                      ("Down", goRight),
                      ("Left", goUp),
                      ("Right", goDown)]
                else [("Up", goUp),
                      ("Down", goDown),
                      ("Left", goLeft),
                      ("Right", goRight)]

data MainWindow ui = MainWindow { myUi :: UiRef ui
                                , myWindow :: Window
                                , myInfoLine :: InfoLine ui
                                , myGoban :: Goban ui
                                }

instance UiCtrl ui => UiView (MainWindow ui) where
  onCursorChange mainWindow cursor = do
    onCursorChange (myInfoLine mainWindow) cursor
    onCursorChange (myGoban mainWindow) cursor

create :: UiCtrl ui => UiRef ui -> IO (MainWindow ui)
create uiRef = do
  window <- windowNew
  windowSetDefaultSize window 640 480

  -- TODO Don't quit if other windows are open.
  -- TODO Return false??
  on window deleteEvent $ liftIO mainQuit >> return False

  on window keyPressEvent $ do
    key <- eventKeyName
    mods <- eventModifier
    let maybeAction = Map.lookup key keyNavActions
    if null mods && isJust maybeAction
      then liftIO $ void $ readUiRef uiRef >>= fromJust maybeAction
      else case key of
        -- Write a list of the current node's properties to the console.
        "t" -> liftIO $ do
          cursor <- readCursor =<< readUiRef uiRef
          print $ nodeProperties $ cursorNode cursor
        -- Draw a tree rooted at the current node to the console.
        "T" -> liftIO $ do
          cursor <- readCursor =<< readUiRef uiRef
          putStrLn $ drawTree $ flip unfoldTree (cursorNode cursor) $ \node ->
            (show $ nodeProperties node, nodeChildren node)
        _ -> return ()
    return True

  boardBox <- vBoxNew False 0
  containerAdd window boardBox

  infoLine <- InfoLine.create uiRef
  boxPackStart boardBox (InfoLine.myLabel infoLine) PackNatural 0

  goban <- Goban.create uiRef
  boxPackStart boardBox (Goban.myDrawingArea goban) PackGrow 0

  return MainWindow { myUi = uiRef
                    , myWindow = window
                    , myInfoLine = infoLine
                    , myGoban = goban
                    }

-- | Makes a 'MainWindow' visible.
display :: MainWindow ui -> IO ()
display = widgetShowAll . myWindow
