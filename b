#!/usr/bin/env bash
set -e

args=''
test=false
with_nix=false

for var in "$@"
do
  if [[ $var == "-t" ]]; then
    test=true
  elif [[ $var == "--nix" ]]; then
    with_nix=true
  else
    args="$args $var"
  fi
done

if [[ $no_nix == true ]]; then
  args="$args --nix"
fi

stack build $args --ghc-options="+RTS -A256m -n2m -RTS" --test --no-run-tests --no-haddock-deps --bench --no-run-benchmarks --jobs=4 --dependencies-only

stack build $args --fast --ghc-options="+RTS -A256m -n2m -RTS" --test --no-run-tests --no-haddock-deps --bench --no-run-benchmarks --jobs=4 2>&1 | perl -pe '$|++; s/(.*) Compiling\s([^\s]+)\s+\(\s+([^\/]+).*/\1 \2/p' | grep -E --color "(^.*warning.*$|^.*error.*$|^    .*$)|"

if [[ $test == true ]]; then
  stack build $args --fast --ghc-options="+RTS -A256m -n2m -RTS" --test --no-haddock-deps --bench --no-run-benchmarks --jobs=4
fi
