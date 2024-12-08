
# Languages

A repo for collaboratively building small benchmarks to compare languages.
If you have a suggestion for improvement: PR!
If you want to add a language: PR!

## Running

To run one of the benchmarks:

1. `cd` into desired benchmark directory (EG `$ cd loops`)
2. Compile by running `$ ../compile.sh`
3. Run via `$ ../run.sh`.
  You should see output something like:
  
  ```
  $ ../run.sh

  Benchmarking Zig
  Benchmark 1: ./zig/code 40
    Time (mean ± σ):     513.9 ms ±   2.9 ms    [User: 504.5 ms, System: 2.6 ms]
    Range (min … max):   510.6 ms … 516.2 ms    3 runs


  Benchmarking C
  Benchmark 1: ./c/code 40
    Time (mean ± σ):     514.0 ms ±   1.1 ms    [User: 505.6 ms, System: 2.8 ms]
    Range (min … max):   513.2 ms … 515.2 ms    3 runs


  Benchmarking Rust
  Benchmark 1: ./rust/target/release/code 40
    Time (mean ± σ):     514.1 ms ±   2.0 ms    [User: 504.6 ms, System: 3.1 ms]
    Range (min … max):   512.4 ms … 516.3 ms    3 runs

  ...
  ```

4. For good measure, execute `$ ../clean.sh` when finished.

Hyperfine is used to warm, execute, and time the runs of the programs.

## Adding

To add a language:

1. Select the benchmark directory you want to add to (EG `$ cd loops`)
2. Create a new subdirectory for the language (EG `$ mkdir rust`)
3. Implement the code in the appropriately named file (EG: `code.rs`)
4. If the language is compiled, add appropriate command to `../compile.sh` and `../clean.sh`
5. Add appropriate line to `../run.sh`

You are also welcome to add new top-level benchmarks dirs

# Available Benchmarks

Each benchmark exists in a subdirectory with the corresponding name.

## loops

A simple, not-super-useful program that does a total of 1 billion loop iterations, with some addition and mod operations for each.
The idea with this is to emphasizes loop, conditional, and basic math performance.

## fibonacci

This program computes the sum of the first N fibonacci numbers.
Each fibonacci number is computed using a naive recursive solution.
Submissions using faster tail-recursion or iterative solutions will not not be accepted.
Emphasizes function call overhead, stack pushing / popping, and recursion.

## levenshtein

This program computes the levenshtein distance between all of the strings provided on the command line.
It prints out the total number of strings compared for distance, and the lowest distance score of all comparisons.
This program emphasizes array/string access and basic looping and conditionals.

# Corresponding visuals

Several visuals have been published based on the work here.
More will likely be added in the future, as this repository improves:

- https://benjdd.com/languages
- https://benjdd.com/languages2
