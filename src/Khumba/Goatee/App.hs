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

-- | Properties of Goatee the application.
module Khumba.Goatee.App (
  applicationName,
  applicationCopyright,
  applicationWebsite,
  applicationAuthors,
  ) where

-- | A string containing the name of this application, @\"Goatee\"@.
applicationName :: String
applicationName = "Goatee"

-- | A user-presentable copyright message.
applicationCopyright :: String
applicationCopyright = "Copyright 2014 Bryan Gardiner"

-- | The home page for Goatee on the web.
applicationWebsite :: String
applicationWebsite = "http://khumba.net/projects/goatee"

-- | A list of contributors to Goatee.
applicationAuthors :: [String]
applicationAuthors = ["Bryan Gardiner <bog@khumba.net>"]
