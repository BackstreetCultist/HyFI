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

val=$(./HyFI $problemInstance $seed $limit | tee /dev/tty | tail -1)
echo $val
