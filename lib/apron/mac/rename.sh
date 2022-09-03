#!/bin/sh

for f in *.dylib; do
    mv -- "$f" "${f%.dylib}.so"
done
