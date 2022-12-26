#!/bin/sh

kelinci_jar_path="$HOME/Documents/workspace/qfuzz/tool/instrumentor/build/libs/kelinci.jar"
prefix="/mnt/c/Users/tianh/Documents/workspace/brbo2-impl/src/main/java/brbo/fuzz"
bin_dir="$prefix/bin"
bin_instr_dir="$prefix/bin-instr"

echo "Compiling"
set +x
# Java source files are under src/
# Binaries are stored into bin/
rm -rf $bin_dir
mkdir $bin_dir
javac -cp ".:$kelinci_jar_path" "$@" -d $bin_dir

echo "Instrumenting"
# Instrumented binaries are stored in bin-instr/
rm -rf $bin_instr_dir
mkdir $bin_instr_dir
java -cp ".:$kelinci_jar_path" edu.cmu.sv.kelinci.instrumentor.Instrumentor -mode LABELS -i $bin_dir -o $bin_instr_dir -skipmain
