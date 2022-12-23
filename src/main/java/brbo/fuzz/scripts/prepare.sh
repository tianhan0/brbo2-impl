#!/bin/sh

kelinci_jar_path="/root/qfuzz/tool/instrumentor/build/libs/kelinci.jar"

echo "Compiling"
# Java source files are under src/
# Binaries are stored into bin/
rm -rf bin/
mkdir bin/
javac -cp ".:$kelinci_jar_path:lib/*" *.java -d bin

echo "Instrumenting"
# Instrumented binaries are stored in bin-instr/
rm -rf bin-instr/
mkdir bin-instr/
java -cp ".:$kelinci_jar_path:lib/*" edu.cmu.sv.kelinci.instrumentor.Instrumentor -mode LABELS -i ./bin/ -o ./bin-instr -skipmain
