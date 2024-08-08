# StaticLint.jl

StaticLint is a static code analyzer for Julia. It searches for patterns in Julia source code, such patterns aiming to indicate issues and deserve to be reported to the end-user.

## Installing and Running StaticLint 

Installing and running StaticLint.jl is easy. You just need to clone this repository _outside_ the Julia project you would like to analyze. Although you add it as a dependency, there is no good reason to mess up with your local `Project.toml` and `Manifest.toml`. 

## Basic usage

There are several ways to use StaticLint.jl. Here are a few usage examples:

```Julia
StaticLint.run_lint_on_text("function f() @async 1 + 2 end ");
---------- /var/folders/nz/1c4rst196ws_18tjtfl0yb980000gn/T/jl_1QHeJ2vm1U.jl
Line 1, column 14: Use `@spawn` instead of `@async`. /var/folders/nz/1c4rst196ws_18tjtfl0yb980000gn/T/jl_1QHeJ2vm1U.jl
1 potential threat is found: 1 violation and 0 recommendation
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

```Julia
StaticLint.run_lint("/Users/alexandrebergel/Documents/RAI/raicode13/src/RAICode.jl")
```

Note that files directly and indirectly included by `RAICode.jl` are also analyzed.

When a directory is provided to `run_lint`, then StaticLint will look for Julia files. E.g.,

```Julia
StaticLint.run_lint("/Users/alexandrebergel/Documents/RAI/raicode13/src/")
```

The expression above outputs 1928 potential threats.

## Contributing to StaticLint.jl

You may want to contribute to StaticLint.jl for many reasons. Here are a few of them:

 - _A rule needs to be better documented_. It is easy to do so: create a PR to this repository that improves one of the rules defined [HERE](https://github.com/RelationalAI/StaticLint.jl/blob/main/src/linting/extended_checks.jl). This `extended_checks.jl` file contains all the RAI-specific rules. 
 - _A new rule has to be defined_. As our system grows and evolves, new rules may have to be defined. The beginning of the file [extended_checks.jl](https://github.com/RelationalAI/StaticLint.jl/blob/main/src/linting/extended_checks.jl) and the section below detail this process. You can always ask `@Alexandre Bergel` on Slack for assistance. Create a new PR with the rule. 

## Lint rules

Several RAI-specific and generic rules are verified on Julia source code.
A number of Julia keywords are known to be [either incompatible or dangerous when committed into raicode](https://relationalai.atlassian.net/browse/RAI-5839). \
The Lint rules available to be run on Julia source code may be found in this [FILE](https://github.com/RelationalAI/StaticLint.jl/blob/main/src/linting/extended_checks.jl).

Adding a new rule is easy. Only the file `src/linting/extended_checks.jl` has to be modified. You need to follow the steps:
1. Create a subtype of `ExtendedRule`, e.g., `struct Async_Extention <: ExtendedRule end`. Lint rules are dynamically looked up by looking at subtypes of `ExtendedRule`.
2. Create an overload of `check` to perform the actual check.

Here is an example of a `check`:

```Julia
check(::Async_Extention, x::EXPR) = generic_check(x, "@async hole_variable", "Use `@spawn` instead of `@async`.")
```

The `generic_check` function takes as a second parameter the expression to be searched. The template string `"@async hole_variable"` means that the expression `x` will be matched against the template. The pseudo variable `hole_variable` matches everything. In case you want to match any arbitrary number of arguments, you can use `hole_variable_star` (look at the test for concrete examples).

If the expression `x` does match the template, then the expression is marked with the error message and used as an output.

In case the expression must be matched in a particular context, e.g., only with a `const` expression, then you can use a `markers`, e.g.,
```
function check(::NThreads_Extention, x::EXPR, markers::Dict{Symbol,Symbol})
    # Threads.nthreads() must not be used in a const field, but it is allowed elsewhere
    haskey(markers, :const) || return
    generic_check(x, "Threads.nthreads()", "`Threads.nthreads()` should not be used in a constant variable.")
end
```

The different markers currently supported are:

| Marker  | Value  |
|:------------- |:---------------|
| `:const`        | Const variable name  |
| `:function`         | Function name          |
| `:macrocall`         | Macro name          |
| `:filename`         | Path and name of the analyzed file          |

If you wish to run a particular rule only in a directory, you could do:

```
function check(::NThreads_Extention, x::EXPR, markers::Dict{Symbol,Symbol})
    isnothing(match(r".*/myfolder/.*", markers[:filename])) || return
    generic_check(x, "Threads.nthreads()", "`Threads.nthreads()` should not be used in a constant variable.")
end
```

This will run the `"Threads.nthreads()"` described earlier in all folders expect in `myfolder`.


## Locally disabling StaticLint

StaticLint can be locally disabled. For now, only for a given line. E.g.,

```Julia
function f1()
    # The following line will not emit an error
    @async 1 + 2 # lint-disable-line
end

function f2()
    # lint-disable-next-line
    @async 1 + 2
    @async 1 + 2 # This line will emit an error
end
```

A specific rule can be locally disabled using `lint-disable-next-line:` taking as an argument
the message that has to be ignored. Consider this example:

```Julia
function f()
    # lint-disable-next-line: Use `@spawn` instead of `@async`.
    @async 1 + 1
end
```

The instruction `@async 1 + 1` raises the error: Use `@spawn` instead of `@async`.
Providing this error msg to the comment `lint-disable-next-line:` disabled it.

Note that it is not necessary to have the full message. The beginning of it is enough. As
such, the code above is equivalent to:

```Julia
function f()
    # lint-disable-next-line: Use `@spawn` instead
    @async 1 + 1
end
```


## Integration with GitHub Action
In addition to being run locally, as described above, StaticLint can be run via GitHub Action. When a PR is created, StaticLint is run on the files modified in this PR and the result is posted as a comment.
Only one report of StaticLint is posted in a PR, and it gets updated at each commit.

## Fork
This repository is a fork of https://github.com/julia-vscode/StaticLint.jl . The decision to fork this project instead of directly contributing to it was not taken lightly. First, the julia-vscode/StaticLint.jl is not designed to be easily and modularly extended. As such using the original StaticLint with our RAI-specific rules was not an easy or even feasible task.
