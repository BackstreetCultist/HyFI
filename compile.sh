#!/bin/bash

# Get problem name
echo "Please select problem to run: "
read problemName
echo Loading $problemName

# Copy to bin folder and move in
rm -rf bin/
mkdir bin/
cp -r src/* ./bin/
cd bin/main/Haskell/tech/charliewilkins/hyfi

# Does specified problem exist?
if [ -d ProblemModules/$problemName ]
then
    echo "Problem found"
else
    echo "ERROR: Problem Module $problemName does not seem to exist"
    exit 1
fi

# Modify Solution.hs to use $problemName
currentProblemName=$(grep "import ProblemModules" Solution/Main.hs | cut -d . -f 2 | head -1)
echo "Original problem: $currentProblemName"
echo "Replacing $currentProblemName with $problemName"
sed -i "s/$currentProblemName/$problemName/g" Solution/Main.hs

# Compile and move out
ghc -threaded HyFI.hs
cd ../../../../../
cp main/Haskell/tech/charliewilkins/hyfi/HyFI .
mkdir -p ProblemModules/$problemName/
cp -r main/Haskell/tech/charliewilkins/hyfi/ProblemModules/$problemName/Instances/ ./ProblemModules/$problemName/
rm -rf main/
rm -rf test/
