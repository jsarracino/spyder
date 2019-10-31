#!/usr/bin/env bash

benchpath=test/bench/spy/benchs
spypath=~/spyder
currpath=$(pwd)



pushd $spypath 1> /dev/null

./Script.hs -b Spy -i $benchpath/$1

popd 1> /dev/null