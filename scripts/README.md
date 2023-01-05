# Example Usage

## Run from End to End

### On Ubuntu Server or Mac

```shell
~/brbo2-impl$ python3 scripts/fuzz+decompose.py \
      --input src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine2.java \
      --qfuzz $HOME/Documents/workspace/qfuzz/ \
      --brbo2 $HOME/Documents/workspace/brbo2-impl/ \
      --brbo $HOME/Documents/workspace/brbo-impl/
```

## Generate Interesting Inputs via QFuzz

### In WSL

Run:
```shell
~/brbo2-impl$ ./scripts/run.sh fuzz -t 180 -d src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine3.java --qfuzz ~/Documents/workspace/qfuzz_docker/ -o src/main/java/brbo/fuzz/
```

Test and Debug:
```shell
~/brbo2-impl$ ./scripts/run.sh fuzz -o src/main/java/brbo/fuzz/ --dry -t 3 -d src/main/java/brbo/benchmarks/sas22/string/apache/lang3/DiffResultToString.java && grep -B 4 -RnIi "observations:" output/fuzz/kelinci_output.txt
```

### In Docker (on Mac M2) 

Run:
```shell
docker run --platform linux/amd64 -v $HOME/Documents:/root/Documents -it --rm yannicnoller/qfuzz
docker run --platform linux/amd64 -v $HOME/Documents:/root/Documents -it --rm issta-23

~/brbo2-impl$ ./scripts/run.sh fuzz -t 7 -d src/main/java/brbo/benchmarks/sas22/string/guava/LenientFormat.java -o src/main/java/brbo/fuzz --dry --qfuzz $HOME/qfuzz/
```

Save a docker image:
```shell
docker commit 828d5aee19c2 issta-23:1.0 
```

Open an additional terminal:
```shell
docker exec -it 85baa1e7449e bash
```

Compile ICRA:
```shell
# Ensure ocamlfind can find the installed packages
eval $(opam config env)
```

## Selectively Decompose Programs

Run in WSL:
```shell
~/brbo-impl$ ./scripts/run_without_deps.sh --directory output/decomposed/stringutils/GetDigits.java  -a transparent
```