# Example Usage

## Generate Interesting Inputs via QFuzz

Run in WSL:
```shell
~/brbo2-impl$ ./scripts/run_without_deps.sh fuzz -t 180 -d src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine3.java --qfuzz ~/Documents/workspace/qfuzz_docker/ -o src/main/java/brbo/fuzz/
```

Run in Docker (on Mac M2):
```shell
docker run --platform linux/amd64 -v $HOME/Documents:/root/Documents -it --rm yannicnoller/qfuzz

~/brbo2-impl$ ./scripts/run_without_deps.sh fuzz -t 7 -d src/main/java/brbo/benchmarks/sas22/string/guava/LenientFormat.java -o src/main/java/brbo/fuzz --dry --qfuzz $HOME/qfuzz/
```

## Selectively Decompose Programs

```shell

```