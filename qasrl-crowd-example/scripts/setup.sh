#!/bin/bash

# Setup script for QASRL project.
# This script is idempotent; don't be afraid to run it multiple times.

# NOTE: If you already have any of the datasets somewhere, go just symlink
# resources/<dirname> to it (for whatever dirname it expects in this script)
# so it doesn't have to be downloaded again.

BASE=`dirname $0`/..
pushd $BASE

if [ ! -e "datasets/wiktionary/" ]
then
    read -p $'Download the Wiktionary data? [y/N]\n' answer
    case ${answer:0:1} in
        y|Y )
            wget \
                --no-check-cert https://www.dropbox.com/s/60hbl3py7g3tx12/wiktionary.tar.gz?dl=1 \
                -O wiktionary.tar.gz
            tar zxvf wiktionary.tar.gz
            rm wiktionary.tar.gz
            mv wiktionary datasets/wiktionary
            ;;
        * )
            echo "Skipping Wiktionary. Run setup.sh again if you change your mind."
            ;;
    esac
fi

popd
