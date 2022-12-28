# Example Usage

## Generate Interesting Inputs via QFuzz

Run in WSL:
```shell
~/brbo2-impl$ ./scripts/run.sh fuzz -t 180 -d src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine3.java --qfuzz ~/Documents/workspace/qfuzz_docker/ -o src/main/java/brbo/fuzz/
```

Run in Docker (on Mac M2):
```shell
docker run --platform linux/amd64 -v $HOME/Documents:/root/Documents -it --rm yannicnoller/qfuzz

~/brbo2-impl$ ./scripts/run.sh fuzz -t 7 -d src/main/java/brbo/benchmarks/sas22/string/guava/LenientFormat.java -o src/main/java/brbo/fuzz --dry --qfuzz $HOME/qfuzz/
```

Test and Debug in WSL:
```shell
~/brbo2-impl$ ./scripts/run.sh fuzz -o src/main/java/brbo/fuzz/ --dry -t 3 -d src/main/java/brbo/benchmarks/sas22/string/apache/lang3/DiffResultToString.java && grep -B 4 -RnIi "observations:" output/fuzz/kelinci_output.txt
```

## Selectively Decompose Programs

Run in WSL:
```shell
~/brbo-impl$ ./scripts/run_without_deps.sh --directory output/decomposed/stringutils/GetDigits.java  -a transparent
```