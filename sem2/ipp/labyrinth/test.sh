#!/bin/bash
RED="\x1B[31m"
GREEN="\x1B[32m"
NORMAL="\x1B[37m"
YELLOW="\x1B[33m"
MAGENTA="\x1B[35m"

valgrind_flags="--quiet --log-fd=69 --error-exitcode=123 
--leak-check=full --show-leak-kinds=all --errors-for-leak-kinds=all"


for f in $2*.in
do 
    printf $YELLOW"test for: $f\n"$NORMAL
    valgrind $valgrind_flags ./$1 <"$f" 1>printf.out 2>printf.err 69> /dev/null
    if (($?==123)); then 
        printf $MAGENTA"MEMORY ERROR\n"$NORMAL
    fi
    printf "OUTS: "
    if diff "printf.out" "${f%in}out" >/dev/null 2>&1; then
        printf $GREEN"SUPERB\n"$NORMAL
    else
        printf $RED"WRONG ANSWER\n"$NORMAL
        mistake_array[wrong_answers]=$f
    fi
    printf "ERRORS: "
    if diff "printf.err" "${f%in}err" >/dev/null 2>&1; then
        printf $GREEN"SUPERB\n"$NORMAL 
    else
        printf $RED"WRONG ANSWER\n"$NORMAL
        mistake_array[wrong_answers]=$f
    fi
    printf "\n"
done
rm printf.out printf.err