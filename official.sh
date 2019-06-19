#!/bin/bash

rm -rf state
git clone --depth 1 https://github.com/aelve/guide-database.git
mv guide-database state
(cd state; zstd -d *.zst)
