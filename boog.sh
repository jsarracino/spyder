#!/bin/sh

TMP=$1
# TMP=`mktemp withlibXXX`
# mv $TMP $TMP.bpl
# TMP=$TMP.bpl
# cp 'lib.bpl' $TMP
# cat $1 >> $TMP
mono -O=all boogie/Binaries/Boogie.exe $TMP
# rm $TMP