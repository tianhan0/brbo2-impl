#!/bin/sh

kelinci_jar_path="/root/qfuzz/tool/instrumentor/build/libs/kelinci.jar"

# The argument to this script is the fully qualified class name of the driver
# Example usage: ./server.sh brbo.fuzz.DriverGreedy
java -cp  ".:$kelinci_jar_path:bin-instr/" edu.cmu.sv.kelinci.Kelinci -K 100 "$@" @@