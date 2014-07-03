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

-- | Common definitions for a renderer that supports failure.
module Khumba.Goatee.Sgf.Renderer (
  Render,
  runRender,
  rendererOf,
  ) where

import Control.Monad.Except (Except, catchError, runExcept, throwError)
import Control.Monad.Writer (WriterT, execWriterT)

type Render = WriterT String (Except String)

-- | Returns either the rendered result on the right, or a message describing a
-- failure on the left.
runRender :: Render a -> Either String String
runRender = runExcept . execWriterT

-- | Wraps a renderer in an exception handler that, when the renderer or
-- something it calls fails, will add context about this renderer's invocation
-- to the failure message.
rendererOf :: Show a => String -> (a -> Render ()) -> a -> Render ()
rendererOf description renderer value = catchError (renderer value) $ \message ->
  throwError $
  message ++ "\n    while trying to render " ++ description ++ " from " ++ show value ++ "."
