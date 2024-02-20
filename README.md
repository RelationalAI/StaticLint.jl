# StaticLint.jl

StaticLint is a static code analyzer for Julia. It runs some rules on Julia source code. 

## Basic usage

There are several ways to use StaticLint.jl. Here are a few usage examples:

```Julia
StaticLint.run_lint_on_text("function f() @async 1 + 2 end ");
---------- /var/folders/nz/1c4rst196ws_18tjtfl0yb980000gn/T/jl_m34buxG5sl.jl
Line 1, column 14: Macro @spawn should be used instead of @async. at offset 13 of /var/folders/nz/1c4rst196ws_18tjtfl0yb980000gn/T/jl_m34buxG5sl.jl
1 potential threat is found
----------
```

Replacing `@async` by `@spawn` make StaticLint happy:

```Julia
julia> StaticLint.run_lint_on_text("function f() @spawn 1 + 2 end ");
---------- /var/folders/nz/1c4rst196ws_18tjtfl0yb980000gn/T/jl_gbkLM58LEL.jl
No potential threats were found.
----------
```

StaticLint can be run on a file:
```
StaticLint.run_lint("/Users/alexandrebergel/Documents/RAI/raicode13/src/RAICode.jl")
```
Note that files directly and indirectly included by `RAICode.jl` are also analyzed.

When a directory is provided to `run_lint`, then StaticLint will look for Julia files. E.g., 
```
StaticLint.run_lint("/Users/alexandrebergel/Documents/RAI/raicode13/src/")
```
output 1928 potential threats.

## Lint rules

Several RAI-specific and generic rules are verified on Julia source code. 
A number of Julia keywords are known to be [either incompatible or dangerous when committed into raicode](https://relationalai.atlassian.net/browse/RAI-5839). The Lint rules available to be run on source code may be found in this [FILE](https://github.com/RelationalAI/StaticLint.jl/blob/main/src/linting/extended_checks.jl). 






## Adding new rules


## Integration with GitHub Action
In addition to being run locally, as described above, StaticLint can be run via GitHub Action. When a PR is created, StaticLint is run on the files modified in this PR and the result is posted as a comment.
Only one report of StaticLint is posted in a PR, and it gets updated at each commit.

## Fork 
This repository is a fork of https://github.com/julia-vscode/StaticLint.jl . The decision to fork this project instead of directly contributing to it was not taken lightly. First, the julia-vscode/StaticLint.jl is not designed to be easily and modularly extended. As such using the original StaticLint with our RAI-specific rules was not an easy or even feasible task.

