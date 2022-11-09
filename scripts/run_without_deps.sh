#!/bin/bash

# This script should be executed from the root directory of this project via `./scripts/run.sh`

# Create output directory
mkdir -p output/
mkdir -p output/cfg
mkdir -p output/amortizations

# Machine-dependent path configurations
brbo_jar="./target/scala-2.12/brbo2-impl.jar:$HOME/Desktop/brbo2-impl.jar" # Assume this jar file only contains the code of brbo

# Jars dependency
# lib_jars="lib/com.microsoft.z3.jar"

classpath=".:$brbo_jar:$lib_jars"

set -x
time java -Dlog4j.configurationFile="src/resources/log4j2.properties" -cp $classpath brbo.BrboMain "${@:1}"
