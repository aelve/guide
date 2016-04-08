#!/bin/bash
set -ev

# Test that the official database can be loaded

if [ -d "state" ]; then
  mv state state-old
fi

git clone --depth 1 https://github.com/aelve/guide-database.git
mv guide-database state

cabal build
dist/build/guide/guide --dry-run

rm -rf state

if [ -d "state-old" ]; then
  mv state-old state
fi
