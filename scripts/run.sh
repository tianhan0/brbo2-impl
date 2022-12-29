#!/bin/bash

# This script should be executed from the root directory of this project

# Create the output directory
mkdir -p output/
mkdir -p output/cfg
mkdir -p output/amortizations

# Machine-dependent path configurations
brbo_jar="./target/scala-2.12/brbo2-impl.jar"

# Jars dependency
# lib_jars="lib/com.microsoft.z3.jar"
# tools.jar is needed for JDK 8, but not JDK 11
tools_jar_wsl="$HOME/.sdkman/candidates/java/current/lib/tools.jar"
tools_jar_docker="/usr/lib/jvm/java-8-openjdk-amd64/lib/tools.jar"

classpath=".:$brbo_jar:$tools_jar_wsl:$tools_jar_docker"

set -x
time java -Dlog4j.configurationFile="src/resources/log4j2.properties" -cp $classpath brbo.BrboMain "${@:1}"
