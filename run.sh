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

./HyFI $problemInstance $seed $limit
