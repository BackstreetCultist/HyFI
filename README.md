# Hyper-Heuristic Functional Interface

HyFI is a powerful tool for the solving of computational optimisation problems.
A Hyper-Heuristic Genetic Algorithm (HHGA) is used to generate search heuristics,
which are then applied within the search space.
These heuristics are 'built' and applied in Java.

HyFI integrates its Haskell and Java components using the [JavaCPP library](https://github.com/bytedeco/javacpp).

The following problems are currently implemented, and can be solved by HyFI:

 - Boolean Satisfiability (SAT)
 - One-Dimensional Bin Packing
 - Permutation Flow Shop
 - Personnel Scheduling

## Running

The system arrives fully Dockerised for dependency management.
The code can also be invoked manually with the following command:

    ghci src/main/Haskell/tech/charliewilkins/hyfi/*.hs src/main/Haskell/tech/charliewilkins/hyfi/**/*.hs src/main/Haskell/tech/charliewilkins/hyfi/**/**/*.hs src/main/Haskell/tech/charliewilkins/hyfi/**/**/**/*.hs

## Testing
Haskell property tests built with QuickCheck are included,
and can be run with the following commands:

    ghci src/test/Haskell/tech/charliewilkins/hyfi/*.hs src/test/Haskell/tech/charliewilkins/hyfi/**/*.hs src/main/Haskell/tech/charliewilkins/hyfi/*.hs src/main/Haskell/tech/charliewilkins/hyfi/**/*.hs src/main/Haskell/tech/charliewilkins/hyfi/**/**/*.hs src/main/Haskell/tech/charliewilkins/hyfi/**/**/**/*.hs

## Implementing 
