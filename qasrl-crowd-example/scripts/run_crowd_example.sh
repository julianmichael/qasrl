#!/bin/bash

BASE=`dirname $0`/..
pushd $BASE
{ echo ":load scripts/crowd_example.scala" & cat <&0; } | sbt "project exampleJVM" console
popd
