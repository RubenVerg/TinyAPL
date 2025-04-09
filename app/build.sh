#!/usr/bin/env bash

set -e

if [[ $PWD == */app ]]; then
	echo "build.sh must be ran in the root tinyapl directory"
	exit 1
fi

rm -rf app/dist
mkdir app/dist

echo "Compiling executable"

wasm32-wasi-cabal build exe:tinyapl

cp $(wasm32-wasi-cabal list-bin tinyapl | tail -n1) app/dist/tinyapl.wasm
