#!/bin/bash
IFS='
'
sf_present=false
rf_present=false

# read the options"
delay=0
unroll=3
narrow=0

# extract options and their arguments into variables.
options=":d:,:u:,:n:,h,:s:,:r:"
while getopts $options opt; do
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

    s)
        sf_present=true
        SOURCE=$OPTARG
        ;;

    r)
        rf_present=true
        RESULT=$OPTARG
        ;;
    h)
        echo "Usage: [options] [argument] ..."
        echo "Options: "
        echo "-s Name                 Folder's name where source files are located. Required option."
        echo "-r Name                 Result's folder name. Required option."
        echo "-d N                    N iterations for loop's widening delay."
        echo "-u N                    N iterations for loop unrolling."
        echo "-n N                    N iterations for loop's fixed point narrowing."
        echo "-h                      Print this message."
        echo " "
        echo "This script targets Unix's OS."
        echo "Report bugs to Manyanda Chitimbo <chitson1990@gmail.com>."
        exit 1
        ;;

    *  ) echo "Unimplemented option: -$OPTARG" >&2;
    exit 1;;

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

#check for presence of required options
if ! $rf_present
then
    echo "Result directory must be included when a directory is specified" >&2
    exit 1
fi

RESULT=$RESULT"/"
RESULT=$(echo $RESULT | perl -pe "s/(\/){2,}/\//g")

if ! [ -d "$RESULT" ]; then
    # result folder creation
    mkdir -p $RESULT
    if [ $? -ne 0 ] ; then
        echo "Error: Permission denied. "
        exit 1;
    else
        echo "Result folder created successfully.";
    fi
fi

interval=$RESULT"interval/"
constant=$RESULT"constant/"
reduced=$RESULT"reduced/"
parity=$RESULT"parity/"
partition_interval=$RESULT"partition-interval/"
partition_constant=$RESULT"partition-constant/"


# Create a folder for each analaysis
mkdir -p $interval $constant $reduced $parity \
$partition_interval $partition_constant

if [ $? -ne 0 ] ; then
    echo "Error: Permission denied. "
    exit 1;
fi


if ! $sf_present
then
    echo "Source directory must be included when a directory is specified" >&2
    exit 1
else
    if ! [ -d "$SOURCE" ]; then
        echo $SOURCE" does not exists." >&2
        exit 1
    fi

    SOURCE=$SOURCE"/"
    SOURCE=$(echo $SOURCE | perl -pe "s/(\/){2,}/\//g")

    LENGTH=$((${#SOURCE}-1))
    SOURCE_1=$(echo ${SOURCE:0:LENGTH})

    for file in $(find $SOURCE_1 -type d | cut -sd / -f 2-)
    do
        sub_directory="/"$file
        #creates subdirectory
        mkdir -p $interval$sub_directory $constant$sub_directory \
               $reduced$sub_directory  $parity$sub_directory \
              $partition_interval$sub_directory $partition_constant$sub_directory
    done
fi

# make clean and make
echo "STARTS MAKE CLEAN"
make clean

echo "MAKE CLEAN FINISHED. STARTS MAKE."
make

echo "MAKE FINISHED. STARTS ANALYSIS."

set -f
extension=".txt"
# analysis of all tests files

for file in $(find $SOURCE_1 -name *.c | cut -sd / -f 2-)
do
    filename=$interval$file$extension

    echo "Starts analysis of: "$SOURCE$file
    ./analyzer.byte -interval $SOURCE$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    filename=$constant$file$extension
    ./analyzer.byte -constant $SOURCE$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    filename=$reduced$file$extension
    ./analyzer.byte -reduced $SOURCE$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    filename=$parity$file$extension
    ./analyzer.byte -parity $SOURCE$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    filename=$partition_interval$file$extension
    ./analyzer.byte -partition-interval $SOURCE$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    filename=$partition_constant$file$extension
    ./analyzer.byte -partition-constant $SOURCE$file -unroll $unroll -delay $delay \
    -narrow $narrow > $filename

    echo "Analysis of: "$SOURCE$file" finished."
    echo " "
done

echo "EXECUTION FINISHED. RESULT FOLDER CREATED."
