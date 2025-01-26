#!/bin/bash
# This script should be used when only modifying the frontend

set -e

if [[ $PWD == */js ]]; then
	echo "build.sh must be ran in the root tinyapl directory"
	exit 1
fi

cd js
tsc
cd ..

echo "Compiled, copying files"

cp -f js/*.html js/dist
cp -f js/*.svg js/dist

cp -f js/dist/* docs/interpreters/latest