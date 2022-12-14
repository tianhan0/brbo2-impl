#!/bin/sh


for filename in $(find $HOME/Documents/workspace/brbo2-impl/output/decomposed -name '*.actual'); do
  echo "Move $filename to $(dirname $filename)/$(basename -s .actual $filename)"
  mv $filename $(basename -s .actual $filename)
done