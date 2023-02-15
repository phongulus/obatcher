#!/bin/bash
LOG="./test_stats.txt"
NOW=$(date)

printf "\n\n%s\n" "$NOW" >> $LOG
if [[ $OSTYPE == 'darwin'* ]]; then
  sysctl -a | grep machdep.cpu >> ./test_stats.txt
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    lscpu >> ./test_stats.txt
fi

make -s >> ./test_stats.txt
