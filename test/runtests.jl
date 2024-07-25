using StaticLint, SymbolServer
using CSTParser, Test
using StaticLint: scopeof, bindingof, refof, errorof, check_all, getenv

server = StaticLint.FileServer();

function get_ids(x, ids=[])
    if StaticLint.headof(x) === :IDENTIFIER
        push!(ids, x)
    elseif x.args !== nothing
        for a in x.args
            get_ids(a, ids)
        end
    end
    ids
end

parse_and_pass(s) = StaticLint.lint_string(s, server)

function check_resolved(s)
    cst = parse_and_pass(s)
    IDs = get_ids(cst)
    [(refof(i) !== nothing) for i in IDs]
end

include(joinpath(@__DIR__, "static_lint_tests.jl"))
include(joinpath(@__DIR__, "rai_rules_tests.jl"))
