#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

cd $DIR
rm ./elpa -R
rm ./packages -R
find . -name '*.elc' -delete
