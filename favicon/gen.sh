#!/bin/bash

# see https://github.com/audreyr/favicon-cheat-sheet

# TODO: make larger icons for Android/iOS

ghc gen.hs

rm -f favicon.ico

for n in 16 24 32 48 64
do
  echo "generating $n x $n"
  ./gen -o "favicon-$n.png" -w "$n" -h "$n"
  optipng -quiet -o7 "favicon-$n.png"
done

convert favicon-*.png favicon.ico

rm -f favicon-*.png gen gen.o gen.hi

echo ""
echo "now move favicon.ico to back/static/"
