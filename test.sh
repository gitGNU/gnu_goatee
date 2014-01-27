#!/bin/sh

cd "$(dirname "$(realpath "$0")")"
source ./cabal.sh
"$CABAL" build && dist/build/test-goatee/test-goatee --hide-successes "$@"
