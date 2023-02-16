#!/bin/bash
LOG="./test_stats.txt"
NOW=$(date)

printf "\n\n%s\n" "$NOW" >> $LOG
if [[ $OSTYPE == 'darwin'* ]]; then
  sysctl -a | grep machdep.cpu >> $LOG
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    lscpu >> $LOG
fi

make -s >> $LOG
