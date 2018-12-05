#!/bin/bash

BASE=`dirname $0`/..
pushd $BASE
{ echo ":load scripts/autocomplete_example.scala" & cat <&0; } | mill -i qasrl.jvm[2.12.6].console
popd
