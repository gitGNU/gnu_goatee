#!/bin/sh

cabal build && dist/build/test-gohs/test-gohs "$@"
