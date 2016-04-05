#!/bin/bash
IFS='
'


# read the options"
delay=0
unroll=3
narrow=0

# extract options and their arguments into variables.
while getopts ":d:,:u:,:n:,h" opt; do
    case $opt in
    d)
        if [ "$OPTARG" -gt "$delay" ]
        then
            delay=$(echo "$OPTARG" | bc)
        fi
          ;;

    u)
        if [ "$OPTARG" -gt "$unroll" ]
        then
            unroll=$(echo "$OPTARG" | bc)
        fi
        ;;

    n)
        if [ "$OPTARG" -gt "$narrow" ]
        then
            narrow=$(echo "$OPTARG" | bc)
        fi
      ;;

    h)
        echo "Usage: [options] [argument] ..."
        echo "Options: "
        echo "-d N                    N iterations for loop's widening delay."
        echo "-u N                    N iterations for loop unrolling."
        echo "-n N                    N iterations for loop's fixed point narrowing."
        echo "-h                      Print this message."
        echo " "
        echo "This program built for Unix's OS."
        echo "Report bugs to Manyanda Chitimbo <chitson1990@gmail.com>."
        exit 1
        ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
    :)
        case $OPTARG in
            d)
                echo "Widening delay analysis option requires an argument." >&2
                ;;
            u)  echo "Loop unrolling analysis option requires an argument." >&2
                ;;
            n) echo "Loop's fixed point narrowing analysis option requires an argument." >&2
                ;;
        esac

        exit 1
      ;;
  esac
done

# make clean and make
echo "STARTS MAKE CLEAN"
make clean

echo "MAKE CLEAN FINISHED. STARTS MAKE."
make

echo "MAKE FINISHED. STARTS EXECUTION."

# result folder creation
result="result/"
rm -rf $result && mkdir $result
interval=$result"interval/"
constant=$result"constant/"
reduced=$result"reduced/"
parity=$result"parity/"
partition_interval=$result"partition-interval/"
partition_constant=$result"partition-constant/"

mkdir $interval $constant $reduced $parity $partition_interval $partition_constant

observation="observation"
mkdir $interval$observation $constant$observation \
       $reduced$observation  $parity$observation \
      $partition_interval$observation $partition_constant$observation

set -f
DIR="tests/"
TXT=".txt"

# analysis of all tests files

for file in $(find "tests" -name *.c | cut -sd / -f 2-)
do
    filename=$interval$file$TXT

    echo "Starts analysis of: "$DIR$file
    ./analyzer.byte -interval $DIR$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    filename=$constant$file$TXT
    ./analyzer.byte -constant $DIR$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    filename=$reduced$file$TXT
    ./analyzer.byte -reduced $DIR$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    filename=$parity$file$TXT
    ./analyzer.byte -parity $DIR$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    filename=$partition_interval$file$TXT
    ./analyzer.byte -partition-interval $DIR$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    filename=$partition_constant$file$TXT
    ./analyzer.byte -partition-constant $DIR$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    echo "Analysis of: "$DIR$file" finished."
    echo " "
done

echo "EXECUTION FINISHED. RESULT FOLDER CREATED."
