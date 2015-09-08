# This file is part of Goatee.
#
# Copyright 2014-2015 Bryan Gardiner
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

{ mkDerivation, base, cairo, containers, directory, filepath
, goatee, gtk, HUnit, lib, mtl, parsec, stdenv

, enableSplitObjs ? null
, forceParallelBuilding ? false
}:
mkDerivation ({
  pname = "goatee-gtk";
  version = "0.3.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cairo containers directory filepath goatee gtk mtl parsec
  ];
  executableHaskellDepends = [ base gtk ];
  testHaskellDepends = [ base HUnit ];
  homepage = "http://khumba.net/projects/goatee";
  description = "A monadic take on a 2,500-year-old board game - GTK+ UI";
  license = stdenv.lib.licenses.agpl3;

  preConfigure =
    if forceParallelBuilding
    then "configureFlags+=\" --ghc-option=-j$NIX_BUILD_CORES\""
    else null;
} // lib.filterAttrs (k: v: v != null) { inherit enableSplitObjs; })
