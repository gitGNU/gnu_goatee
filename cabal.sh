#!/usr/bin/env bash

# This file is part of Goatee.
#
# Copyright 2014 Bryan Gardiner
#
# Goatee is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Goatee is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with Goatee.  If not, see <http://www.gnu.org/licenses/>.

CABAL="$(which cabal 2>/dev/null)"
if test $? -ne 0; then
    ghc Setup || { echo "Couldn't build Setup.hs, aborting."; exit 1; }
    CABAL=./Setup
fi
