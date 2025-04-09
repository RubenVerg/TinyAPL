#!/usr/bin/env bash

set -e

if [[ $PWD == */js ]]; then
	echo "build.sh must be ran in the root tinyapl directory"
	exit 1
fi

rm -rf js/dist
mkdir js/dist

echo "Compiling library"

wasm32-wasi-cabal build tinyapl-js

# out_path=$(find dist-newstyle -name "tinyapl-js.wasm")
out_path=$(wasm32-wasi-cabal list-bin tinyapl-js | tail -n1)
cp $out_path js/dist/tinyapl-js.wasm

echo "Compiled, found $out_path, post-linking"

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs --input "$out_path" --output js/ghc_wasm_jsffi.js

echo "Post-linked, compiling TypeScript"

cd js
tsc
cd ..

echo "Compiled, copying files"

cp js/*.html js/dist
cp js/*.svg js/dist
