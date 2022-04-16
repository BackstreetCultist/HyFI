# Hyper-Heuristic Functional Interface

HyFI is a powerful tool for the solving of computational optimisation problems.
A Hyper-Heuristic Genetic Algorithm (HHGA) is used to generate search heuristics,
which are then applied within the search space.

The following problems are currently implemented, and can be solved by HyFI:

 - Boolean Satisfiability (SAT)
 - One-Dimensional Bin Packing

 Each of these problems is implemented in the relevant Problem Module,
 and an Example PM is also included to walk you through the process of implementing a new problem.
 Guidance for doing so can be found in the *notes.txt* file in the Example directory.

## Running

The system can be invoked using either the *run.sh* or *multiRun.sh* scripts.
Each will prompt you for the problem you wish to solve,
the file path to the instance of that problem (see inside the Problem Modules for which instances are included),
an initial seed and a time limit.
Additionally,
*multiRun.sh* will prompt you for a number of experiments,
which will be logged to *bin/log.txt*.

Please note that due to the way it interacts with teletypes,
*multiRun.sh* will have some lag in its output,
this is not something to be concerned by.

The system arrives fully Dockerised for dependency management [TODO]

### Running with Multiple Processors

The system is designed for parallel processing,
however the two included scripts will run it in single-core mode.
To run with multiple,
use the following command:

    ./compile.sh
    cd bin/
    ./HyFI -RTS [instance] [seed] [time limit] +RTS -N[number of cores]

## Testing
Haskell property tests built with QuickCheck are included,
and can be run with the following commands:

    ghci src/test/Haskell/tech/charliewilkins/hyfi/*.hs src/test/Haskell/tech/charliewilkins/hyfi/**/*.hs src/main/Haskell/tech/charliewilkins/hyfi/*.hs src/main/Haskell/tech/charliewilkins/hyfi/**/*.hs src/main/Haskell/tech/charliewilkins/hyfi/**/**/*.hs src/main/Haskell/tech/charliewilkins/hyfi/**/**/**/*.hs [TODO]
