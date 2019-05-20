#!/bin/bash

CODE=$(curl ipinfo.io 2> /dev/null | awk '/country/ {print substr($2,2,2)}')

echo $CODE
echo $CODE

if [ $CODE != "GB" ]; then
   exit 33
fi
