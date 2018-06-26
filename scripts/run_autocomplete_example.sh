#!/bin/bash

BASE=`dirname $0`/..
pushd $BASE
{ echo ":load scripts/autocomplete_example.scala" & cat <&0; } | mill -i qasrl.jvm.console
popd
