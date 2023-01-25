# Example Usage

## Run from End to End

**On Ubuntu Server or Mac.**: Run

```shell
~/brbo2-impl$ python3 scripts/brbo2.py \
      --input src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine2.java \
      --qfuzz $HOME/Documents/workspace/qfuzz/ \
      --brbo $HOME/Documents/workspace/brbo-impl/ \
      --log output/logs/log16.json \
      > output/logs/log16.txt 2>&1 &
```

### Generate Interesting Inputs via QFuzz

**In WSL.** Run:
```shell
~/brbo2-impl$ ./scripts/run.sh fuzz \
  -t 180 \
  -d src/main/java/brbo/benchmarks/sas22/stac/TemplateEngine3.java \
  --qfuzz ~/Documents/workspace/qfuzz_docker/ \
  -o src/main/java/brbo/fuzz/
```

Test and Debug:
```shell
~/brbo2-impl$ ./scripts/run.sh fuzz \
  -o src/main/java/brbo/fuzz/ \
  --dry \
  -t 3 \
  -d src/main/java/brbo/benchmarks/sas22/string/apache/lang3/DiffResultToString.java \
  && grep -B 4 -RnIi "observations:" output/fuzz/kelinci_output.txt
```

**In Docker (on Mac M2).** Run:
```shell
docker run \
  --platform linux/amd64 \ 
  -v $HOME/Documents:/root/Documents \
  -it --rm \
  yannicnoller/qfuzz
docker run \
  --platform linux/amd64 \
  -v $HOME/Documents:/root/Documents \
  -it --rm \
  issta-23

~/brbo2-impl$ ./scripts/run.sh fuzz \
  -t 7 \
  -d src/main/java/brbo/benchmarks/sas22/string/guava/LenientFormat.java \
  -o src/main/java/brbo/fuzz \
  --dry \ 
  --qfuzz $HOME/qfuzz/
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

### (Selectively) Decompose Programs

**On Mac (M2).** Run:
```shell
[~/Documents/workspace/brbo2-impl] ./scripts/run_deps.sh decompose \
  --threads 6 \
  --debug \
  --algorithm optics \
  --parameter 0.1 \
  --samples 0 \
  --directory src/main/java/brbo/benchmarks/sas22/string/apache/stringutils/ReplaceChars.java \
  --input
```

**In WSL.** Run:
```shell
~/brbo-impl$ ./scripts/run_without_deps.sh \
  --directory output/decomposed/stringutils/GetDigits.java \
  --amortize transparent
```

### Verify (Decomposed) Programs:

**On the Ubuntu server.** Run:
```shell
[~/Documents/workspace/brbo-impl] $ ./scripts/run_deps.sh \
  --directory /root/Documents/workspace/brbo2-impl/output/decomposed/stringutils/Replace.java \
  --amortize transparent \
  --icra-path /root/Documents/workspace/icra/icra \
  --icra-timeout 60
```

## Debug

Run on the SAS'21 docker image from the Ubuntu server:
```shell
docker run -it --rm --privileged sas-artifact-41

root@16144970debc:/home/sas-artifact-41/brbo-impl# ./scripts/run_with_deps_artifact.sh \
  --amortize selective \
  --icra-timeout 60 \
  --directory src/main/java/brbo/benchmarks/containers/stac/TemplateEngine3.java
```

## Transform into Brbo-compatible Programs

```shell
./scripts/run_deps.sh brbo --directory /Users/tianhanlu/Documents/workspace/brbo2-impl/src/main/java/brbo/benchmarks/sas22/string/apache/stringutils/ReplaceChars.java 
```

## Run Baseline

**On the Ubuntu server.** Run:
```shell
~/brbo2-impl$ { python3 scripts/brbo.py \
  --input src/main/java/brbo/benchmarks/sas22/ \
  --brbo $HOME/Documents/workspace/brbo-impl/ \
  --timeout 60 \
  --mode worst && \
  python3 scripts/brbo.py \
  --input src/main/java/brbo/benchmarks/sas22/ \
  --brbo $HOME/Documents/workspace/brbo-impl/ \
  --timeout 60 \
  --mode fully; } \
  > ./output/logs/baseline/log01.txt 2>&1 &
```
Thanks to https://stackoverflow.com/questions/44192376/redirect-outputs-of-multiple-commands-to-a-file.

# Run Experiments

```shell
~/brbo2-impl$ python3 scripts/experiments.py \
  --input src/main/java/brbo/benchmarks/sas22/ \
  --qfuzz $HOME/Documents/workspace/qfuzz/ \
  --brbo $HOME/Documents/workspace/brbo-impl/ \
  --experiment qfuzz \
  --repeat 5
```

```shell
~/brbo2-impl$ python3 scripts/experiments.py \
  --input src/main/java/brbo/benchmarks/sas22/ \
  --qfuzz $HOME/Documents/workspace/qfuzz/ \
  --brbo $HOME/Documents/workspace/brbo-impl/ \
  --experiment timeout \
  --repeat 2
```

# Set Memory Limit (Ubuntu)

Such that icra will not use too much memory, which causes the OS to kill the python scripts:
```shell
(20:16:20) [~/Documents/workspace/brbo2-impl] $ cat /etc/security/limits.conf 
# /etc/security/limits.conf
#
#Each line describes a limit for a user in the form:
#
#<domain>        <type>  <item>  <value>
#
#Where:
#<domain> can be:
#        - a user name
#        - a group name, with @group syntax
#        - the wildcard *, for default entry
#        - the wildcard %, can be also used with %group syntax,
#                 for maxlogin limit
#        - NOTE: group and wildcard limits are not applied to root.
#          To apply a limit to the root user, <domain> must be
#          the literal username root.
#
#<type> can have the two values:
#        - "soft" for enforcing the soft limits
#        - "hard" for enforcing hard limits
#
#<item> can be one of the following:
#        - core - limits the core file size (KB)
#        - data - max data size (KB)
#        - fsize - maximum filesize (KB)
#        - memlock - max locked-in-memory address space (KB)
#        - nofile - max number of open files
#        - rss - max resident set size (KB)
#        - stack - max stack size (KB)
#        - cpu - max CPU time (MIN)
#        - nproc - max number of processes
#        - as - address space limit (KB)
#        - maxlogins - max number of logins for this user
#        - maxsyslogins - max number of logins on the system
#        - priority - the priority to run user process with
#        - locks - max number of file locks the user can hold
#        - sigpending - max number of pending signals
#        - msgqueue - max memory used by POSIX message queues (bytes)
#        - nice - max nice priority allowed to raise to values: [-20, 19]
#        - rtprio - max realtime priority
#        - chroot - change root to directory (Debian-specific)
#
#<domain>      <type>  <item>         <value>
#

#*               soft    core            0
#root            hard    core            100000
#*               hard    rss             10000
#@student        hard    nproc           20
#@faculty        soft    nproc           20
#@faculty        hard    nproc           50
#ftp             hard    nproc           0
#ftp             -       chroot          /ftp
#@student        -       maxlogins       4

# End of file

root - fsize 4000000
root hard as 6000000
```