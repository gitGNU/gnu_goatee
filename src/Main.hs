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

-- | Main module, controls application start-up.
module Main (main) where

import Control.Monad
import Graphics.UI.Gtk
--import Graphics.UI.WX (start)
--import Khumba.Goatee.Sgf
--import Khumba.Goatee.Sgf.Parser
import Khumba.Goatee.Ui.Gtk
--import Khumba.Goatee.Ui.Wx
--import System.Environment (getArgs)

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

{-
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
-}
