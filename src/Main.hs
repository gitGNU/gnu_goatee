module Main (
    main
) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.WX (start)
import Khumba.GoHS.Common
import Khumba.GoHS.Sgf
import Khumba.GoHS.Sgf.Parser
import Khumba.GoHS.Ui.Gtk
import Khumba.GoHS.Ui.Wx
import System.Environment (getArgs)

main :: IO ()
main = mainGtk

mainGtk :: IO ()
mainGtk = do
  args <- initGUI
  if null args
    then void $ startNewBoard Nothing
    else do result <- startFile $ head args
            case result of
              Left msg -> print msg
              _ -> return ()
  mainGUI

mainWx :: IO ()
mainWx = do
  args <- getArgs
  if null args
    then start $ boardFrame $ rootCursor emptyNode
    else do result <- parseFile $ head args
            case result of
              Right collection ->
                -- TODO Don't only choose the first tree in the collection.
                start $ boardFrame $ rootCursor $ head $ collectionTrees collection
              Left err ->
                putStrLn $ "Error loading file: " ++ show err
