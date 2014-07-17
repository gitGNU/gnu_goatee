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

module Main (main) where

import Game.Goatee.Sgf.Board
import Game.Goatee.Sgf.Parser
import Game.Goatee.Sgf.Tree
import Game.Goatee.Ui.Wx
import Graphics.UI.WX (start)
import System.Environment (getArgs)

main :: IO ()
main = do
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
