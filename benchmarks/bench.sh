#!/bin/bash
LOG="./test_stats.txt"
NOW=$(date)

printf "\n\n%s\n" "$NOW" >> $LOG
if [[ $OSTYPE == 'darwin'* ]]; then
  sysctl -n machdep.cpu.brand_string >> ./test_stats.txt
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    cat /proc/info >> ./test_stats.txt
fi

make -s >> ./test_stats.txt
