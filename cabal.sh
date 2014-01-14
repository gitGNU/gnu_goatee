#!/bin/sh

CABAL="$(which cabal 2>/dev/null)"
if test $? -ne 0; then
    ghc Setup || { echo "Couldn't build Setup.hs, aborting."; exit 1; }
    CABAL=./Setup
fi
