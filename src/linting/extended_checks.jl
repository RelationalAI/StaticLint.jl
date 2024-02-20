
## UTILITY FUNCTIONS
function is_hole_variable(x::CSTParser.EXPR)
    return x.head == :IDENTIFIER && x.val in ["hole_variable", "hole_variable_star"]
end

function is_hole_variable_star(x::CSTParser.EXPR)
    return x.head == :IDENTIFIER && x.val == "hole_variable_star"
end

comp(x, y) = x == y
function comp(x::CSTParser.EXPR, y::CSTParser.EXPR)
    (is_hole_variable(x) || is_hole_variable(y)) && return true

    result = comp(x.head, y.head) && x.val == y.val
    !result && return false

    min_length = min(length(x), length(y))

    for i in 1:min_length
        comp(x[i], y[i]) || return false
        (is_hole_variable_star(x[i]) || is_hole_variable_star(y[i])) && return true
    end

    length(x) == length(y) && return true

    if length(x) == min_length
        return is_hole_variable_star(y[min_length + 1])
    end

    if length(y) == min_length
        return is_hole_variable_star(x[min_length + 1])
    end

    return false
end

abstract type ExtendedRule end
struct Async_Extention <: ExtendedRule end
struct Ccall_Extention <: ExtendedRule end
struct Pointer_from_objref_Extention <: ExtendedRule end
struct NThreads_Extention <: ExtendedRule end
struct Finalizer_Extention <: ExtendedRule end

const all_extended_rule_types = InteractiveUtils.subtypes(ExtendedRule)

const check_cache = Dict{String, CSTParser.EXPR}()
const error_msgs = Dict{String, String}()

function generic_check(x::EXPR, template_code::String, error_code)
    get!(check_cache, template_code, CSTParser.parse(template_code))
    # get!(error_msgs, template_code, error_msg)
    oracle = check_cache[template_code]
    if comp(x, oracle)
        seterror!(x, error_code)
    end
end

function check(::Finalizer_Extention, x::EXPR)
    generic_check(x, "finalizer(hole_variable, hole_variable)", ProhibitedFinalizer)
    generic_check(x, "finalizer(x) do hole_variable hole_variable end", ProhibitedFinalizer)
end

# Useful for rules that do not need markers
check(t::Any, x::EXPR, markers::Dict{Symbol,Symbol}) = check(t, x)
check(::Async_Extention, x::EXPR) = generic_check(x, "@async hole_variable", ProhibitedAsyncMacro)
check(::Ccall_Extention, x::EXPR) = generic_check(x, "ccall(hole_variable, hole_variable, hole_variable, hole_variable_star)", ProhibitedCCall)
check(::Pointer_from_objref_Extention, x::EXPR) = generic_check(x, "pointer_from_objref(hole_variable)", ProhibitedPointerFromObjref)

function check(::NThreads_Extention, x::EXPR, markers::Dict{Symbol,Symbol})
    haskey(markers, :const) || return
    generic_check(x, "Threads.nthreads()", ProhibitedNThreads)
end