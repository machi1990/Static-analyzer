#!/bin/bash
IFS='
'
echo "STARTS MAKE CLEAN"
make clean

echo "MAKE CLEAN FINISHED. STARTS MAKE."
make

echo "MAKE FINISHED. STARTS EXECUTION."

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

for file in $(find "tests" -name *.c | cut -sd / -f 2-)
do
filename=$interval$file$TXT

echo "Starts analysis of: "$DIR$file
./analyzer.byte -interval $DIR$file -unroll 3 > $filename

filename=$constant$file$TXT
./analyzer.byte -interval $DIR$file -unroll 3 > $filename

filename=$reduced$file$TXT
./analyzer.byte -interval $DIR$file -unroll 3 > $filename

filename=$parity$file$TXT
./analyzer.byte -interval $DIR$file -unroll 3 > $filename

filename=$partition_interval$file$TXT
./analyzer.byte -interval $DIR$file -unroll 3 > $filename

filename=$partition_constant$file$TXT
./analyzer.byte -interval $DIR$file -unroll 3 > $filename

echo "Analysis of: "$DIR$file" finished."
echo " "
done

echo "EXECUTION FINISHED. RESULT FOLDER CREATED."
