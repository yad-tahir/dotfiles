#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


rm $DIR/elpa -R
rm $DIR/packages -R
find $DIR -name '*.elc' -delete
