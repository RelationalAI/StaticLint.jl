
#################################################################################
# UTILITY FUNCTIONS
#################################################################################
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

#################################################################################
# EXTENDED LINT RULES
#################################################################################
abstract type ExtendedRule end
struct Async_Extention <: ExtendedRule end
struct Ccall_Extention <: ExtendedRule end
struct Pointer_from_objref_Extention <: ExtendedRule end
struct NThreads_Extention <: ExtendedRule end
struct Finalizer_Extention <: ExtendedRule end
struct CFunction_Extension <: ExtendedRule end
struct Semaphore_Extension <: ExtendedRule end
struct Destructor_Extension <: ExtendedRule end
struct ReentrantLock_Extension <: ExtendedRule end
struct SpinLock_Extension <: ExtendedRule end
struct Lock_Extension <: ExtendedRule end

const all_extended_rule_types = InteractiveUtils.subtypes(ExtendedRule)

# template -> EXPR to be compared
const check_cache = Dict{String, CSTParser.EXPR}()

# template -> error_msg
const error_msgs = Dict{String, String}()

function reset_static_lint_caches()
    check_cache = Dict{String, CSTParser.EXPR}()
    error_msgs = Dict{String, String}()
    return nothing
end

function get_oracle_ast(template_code::String)
    get!(check_cache, template_code, CSTParser.parse(template_code))
    return check_cache[template_code]
end

does_match(x::EXPR, template_code::String) = comp(x, get_oracle_ast(template_code))
function generic_check(x::EXPR, template_code::String, error_code)
    error_code isa String && get!(error_msgs, template_code, error_code)
    does_match(x, template_code) && seterror!(x, error_code)
end

# Useful for rules that do not need markers
check(t::Any, x::EXPR, markers::Dict{Symbol,Symbol}) = check(t, x)

# The following function defines rules that are matched on the input Julia source code
# Each rule comes with a pattern that is checked against the abstract syntax tree
function check(::Finalizer_Extention, x::EXPR)
    error_msg = "`finalizer(_,_)` should not be used."
    generic_check(x, "finalizer(hole_variable, hole_variable)", error_msg)
    generic_check(x, "finalizer(hole_variable) do hole_variable hole_variable_star end", error_msg)
end

function check(::Async_Extention, x::EXPR)
    generic_check(x, "@async hole_variable", "Macro `@spawn` should be used instead of `@async`.")
    generic_check(x, "Threads.@async hole_variable", "Macro `@spawn` should be used instead of `@async`.")
end

check(::Ccall_Extention, x::EXPR) = generic_check(x, "ccall(hole_variable, hole_variable, hole_variable, hole_variable_star)", "`ccall` should be used with extreme caution.")
check(::Pointer_from_objref_Extention, x::EXPR) = generic_check(x, "pointer_from_objref(hole_variable)", "`pointer_from_objref` should be used with extreme caution.")

function check(::NThreads_Extention, x::EXPR, markers::Dict{Symbol,Symbol})
    # Threads.nthreads() must not be used in a const field, but it is allowed elsewhere
    haskey(markers, :const) || return
    generic_check(x, "Threads.nthreads()", "`Threads.nthreads()` should not be used in a constant variable.")
end

check(::CFunction_Extension, x::EXPR) = generic_check(x, "@cfunction(hole_variable, hole_variable_star)", "Macro `@cfunction` should not be used.")

check(::Semaphore_Extension, x::EXPR) = generic_check(x, "Semaphore(hole_variable)", "`Semaphore` should be used with extreme caution.")
check(::ReentrantLock_Extension, x::EXPR) = generic_check(x, "ReentrantLock()", "`ReentrantLock` should be used with extreme caution.")

function check(::Destructor_Extension, x::EXPR)
    error_msg = "Destructors should be used with extreme caution."
    generic_check(x, "destructor(hole_variable, hole_variable)", error_msg)
    generic_check(x, "destructor(hole_variable) do hole_variable hole_variable_star end", error_msg)
end

function check(::SpinLock_Extension, x::EXPR)
    msg = "`SpinLock` should be used with extreme caution."
    generic_check(x, "SpinLock()", msg)
    generic_check(x, "Threads.SpinLock()", msg)
    generic_check(x, "Base.Threads.SpinLock()", msg)
end

function check(::Lock_Extension, x::EXPR)
    msg = "`@lock` should be used with extreme caution."
    generic_check(x, "@lock hole_variable hole_variable", msg)
    generic_check(x, "Base.@lock hole_variable hole_variable", msg)
end

