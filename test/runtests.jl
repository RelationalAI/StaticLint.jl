using StaticLint, SymbolServer
using CSTParser, Test
using StaticLint: convert_offset_to_line_from_lines, scopeof, bindingof, refof, errorof, check_all, getenv

include(joinpath(@__DIR__, "rai_rules_tests.jl"))

