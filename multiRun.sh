#!/bin/bash

echo "Hello! Welcome to HyFI"

# Build HyFI
./compile.sh
cd bin/

# Get arguments
echo "Please select problem instance to solve: "
read problemInstance

echo "Please input a numerical seed: "
read seed

echo "Please set time limit in seconds: "
read limit

echo "How many times would you like to run your experiment?"
read count

for i in $(seq 1 $count)
do
    echo "Hello $i"
    newSeed=$(($seed + $i))
    echo "New seed: $newSeed"
    val=$(./HyFI $problemInstance $newSeed $limit | tee /dev/tty | tail -1)
    echo $val
    echo "Instance: $problemInstance Seed: $newSeed Limit: $limit Result: $val" >> log.txt
done
