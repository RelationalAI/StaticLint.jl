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

# @compile_workload begin
#     println("Warming up using path $(@__DIR__)")
#     StaticLint.run_lint(@__DIR__)
# end
# WITH:
# 279.849137 seconds (5.03 G allocations: 359.563 GiB, 8.67% gc time)
# 263.746065 seconds (5.03 G allocations: 359.584 GiB, 7.14% gc time, 0.45% compilation time: 14% of which was recompilation)


# WITHOUT:
# 295.521349 seconds (5.03 G allocations: 359.565 GiB, 7.89% gc time, 0.02% compilation time)
# 283.552941 seconds (5.03 G allocations: 359.563 GiB, 8.01% gc time)

# @time StaticLint.run_lint("/Users/alexandrebergel/Documents/RAI/raicode2")
# @time begin using StaticLint ; StaticLint.run_lint(@__DIR__) end
end