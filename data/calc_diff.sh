#!/usr/bin/env bash

diffpath=~/apted
currpath=$(pwd)



pushd $diffpath 1> /dev/null

java -jar build/libs/apted.jar -f $currpath/$1 $currpath/$2
java -jar build/libs/apted.jar -f $currpath/$1-impl $currpath/$2-impl
java -jar build/libs/apted.jar -f $currpath/$1-spec $currpath/$2-spec

popd 1> /dev/null