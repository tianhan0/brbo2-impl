#!/bin/sh

./scripts/run_without_deps.sh -i -t 1 -g --algorithm optics --parameter 0.1 --samples 10 --directory src/main/java/brbo/benchmarks/sas22/sas21/stac/
./scripts/run_without_deps.sh -i -t 1 -g --algorithm optics --parameter 0.1 --samples 10 --directory src/main/java/brbo/benchmarks/sas22/sas21/string/guava/