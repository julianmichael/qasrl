#!/bin/bash

BASE=`dirname $0`/..
pushd $BASE
{ echo ":load scripts/autocomplete_example.scala" & cat <&0; } | sbt "-Djline.terminal=none" "project qasrlJVM" console
popd
