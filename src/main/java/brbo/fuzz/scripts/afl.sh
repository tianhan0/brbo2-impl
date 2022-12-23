#!/bin/sh

# Initial inputs are stored under in_dir/
AFL_SKIP_CPUFREQ=1 AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1 /root/qfuzz/tool/afl-2.51b-qfuzz/afl-fuzz -i inputs -o fuzzer-out -c quantify -K 100 -S afl -t 999999999 /root/qfuzz/tool/fuzzerside/interface -K 100 @@