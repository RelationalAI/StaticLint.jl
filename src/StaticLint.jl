module StaticLint

using CSTParser: CSTParser, EXPR
import InteractiveUtils
using PrecompileTools: @setup_workload, @compile_workload

mutable struct LintMeta
    error
    LintMeta() = new(nothing)
    LintMeta(v) = new(v)
end

# include("linting/checks.jl")

include("linting/extended_checks.jl")

include("interface.jl")
end