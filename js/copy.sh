#!/usr/bin/env bash

set -e

if [[ $PWD == */js ]]; then
	echo "copy.sh must be ran in the root tinyapl directory"
	exit 1
fi

rm -rf docs/interpreters/latest
cp -r js/dist docs/interpreters/latest
