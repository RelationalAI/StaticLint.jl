
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

comp_value(x, y) = x == y
function comp_value(x::String, y::String)
    is_there_any_star_marker = contains(x, "QQQ") || contains(y, "QQQ")
    !is_there_any_star_marker && return x == y

    contains(x, "QQQ") && contains(y, "QQQ") && error("Cannot both $x and $y have a star marker")
    if contains(x, "QQQ")
        reg_exp = Regex(replace(x, "QQQ" => ".*"))
        return !isnothing(match(reg_exp, y))
    else
        reg_exp = Regex(replace(y, "QQQ" => ".*"))
        return !isnothing(match(reg_exp, x))
    end
end

function comp(x::CSTParser.EXPR, y::CSTParser.EXPR)
    (is_hole_variable(x) || is_hole_variable(y)) && return true

    result = comp(x.head, y.head) && comp_value(x.val, y.val)
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
struct Unlock_Extension <: ExtendedRule end
struct Yield_Extension <: ExtendedRule end
struct Sleep_Extension <: ExtendedRule end
struct Mmap_Extension <: ExtendedRule end
struct Future_Extension <: ExtendedRule end
struct Wait_Extension <: ExtendedRule end
struct Fetch_Extension <: ExtendedRule end
struct Inbounds_Extension <: ExtendedRule end
struct Atomic_Extension <: ExtendedRule end
struct Ptr_Extension <: ExtendedRule end
struct ArrayWithNoType_Extension <: ExtendedRule end
struct Threads_Extension <: ExtendedRule end
struct Generated_Extension <: ExtendedRule end
struct Sync_Extension <: ExtendedRule end
struct RemovePage_Extension <: ExtendedRule end
struct Channel_Extension <: ExtendedRule end
struct Task_Extension <: ExtendedRule end
struct ErrorException_Extension <: ExtendedRule end
struct Error_Extension <: ExtendedRule end
struct Unsafe_Extension <: ExtendedRule end


const all_extended_rule_types = Ref{Any}(InteractiveUtils.subtypes(ExtendedRule))

# template -> EXPR to be compared
const check_cache = Dict{String, CSTParser.EXPR}()

# template -> error_msg
const error_msgs = Dict{String, String}()

function reset_static_lint_caches()
    empty!(check_cache)
    empty!(error_msgs)
    all_extended_rule_types[] = InteractiveUtils.subtypes(ExtendedRule)
    return nothing
end

function get_oracle_ast(template_code::String)
    get!(check_cache, template_code, CSTParser.parse(template_code))
    return check_cache[template_code]
end

does_match(x::EXPR, template_code::String) = comp(x, get_oracle_ast(template_code))
function generic_check(x::EXPR, template_code::String, error_msg)
    error_msg isa String && get!(error_msgs, template_code, error_msg)
    does_match(x, template_code) && seterror!(x, error_msg)
end

function generic_check(x::EXPR, template_code::String)
    keyword = first(split(template_code, ['(', '{', ' ']))
    return generic_check(x, template_code, "`$(keyword)` should be used with extreme caution.")
end


# Useful for rules that do not need markers
check(t::Any, x::EXPR, markers::Dict{Symbol,String}) = check(t, x)

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

function check(::NThreads_Extention, x::EXPR, markers::Dict{Symbol,String})
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

check(::Unlock_Extension, x::EXPR) = generic_check(x, "unlock(hole_variable)")
check(::Yield_Extension, x::EXPR) = generic_check(x, "yield()")
check(::Sleep_Extension, x::EXPR) = generic_check(x, "sleep(hole_variable)")
function check(::Mmap_Extension, x::EXPR)
    generic_check(x, "mmap(hole_variable_star)")
    generic_check(x, "Mmap.mmap(hole_variable_star)", "`mmap` should be used with extreme caution.")
end

check(::Fetch_Extension, x::EXPR) = generic_check(x, "fetch(hole_variable)")
check(::Inbounds_Extension, x::EXPR) = generic_check(x, "@inbounds hole_variable")

function check(::Atomic_Extension, x::EXPR)
    msg = "`Atomic` should be used with extreme caution."
    generic_check(x, "Atomic(hole_variable_star)", msg)
    generic_check(x, "Atomic{hole_variable}(hole_variable_star)", msg)
    generic_check(x, "Threads.Atomic(hole_variable_star)", msg)
    generic_check(x, "Threads.Atomic{hole_variable}(hole_variable_star)", msg)
end

function check(::Future_Extension, x::EXPR)
    generic_check(x, "Future{hole_variable}(hole_variable_star)")
    generic_check(x, "Future(hole_variable_star)")
end

check(::Wait_Extension, x::EXPR) = generic_check(x, "wait(hole_variable)")
check(::Ptr_Extension, x::EXPR) = generic_check(x, "Ptr{hole_variable}(hole_variable)")

function check(::ArrayWithNoType_Extension, x::EXPR, markers::Dict{Symbol,String})
    haskey(markers, :filename) || return
    contains(markers[:filename], "src/Compiler") || return
    generic_check(x, "[]", "Need a specific Array type to be provided.")
end

function check(::Threads_Extension, x::EXPR)
    msg = "`@threads` should be used with extreme caution."
    generic_check(x, "Threads.@threads hole_variable", msg)
    generic_check(x, "@threads hole_variable", msg)
end

check(::Generated_Extension, x::EXPR) = generic_check(x, "@generated hole_variable")

function check(::Sync_Extension, x::EXPR)
    msg = "`@sync` should be used with extreme caution."
    generic_check(x, "@sync hole_variable", msg)
    generic_check(x, "Threads.@sync hole_variable", msg)
end

check(::RemovePage_Extension, x::EXPR) = generic_check(x, "remove_page(hole_variable,hole_variable)")
check(::Channel_Extension, x::EXPR) = generic_check(x, "Channel(hole_variable_star)")
check(::Task_Extension, x::EXPR) = generic_check(x, "Task(hole_variable)")

function check(::ErrorException_Extension, x::EXPR)
    generic_check(
        x,
        "ErrorException(hole_variable_star)",
        "Use custom exception instead of the generic `ErrorException`")
end

function check(::Error_Extension, x::EXPR)
    generic_check(
        x,
        "error(hole_variable)",
        "Use custom exception instead of the generic `error(...)`")
end

function check(::Unsafe_Extension, x::EXPR, markers::Dict{Symbol,String})
    haskey(markers, :function) || return
    isnothing(match(r"_unsafe_.*", markers[:function])) || return
    isnothing(match(r"unsafe_.*", markers[:function])) || return

    generic_check(
        x,
        "unsafe_QQQ(hole_variable_star)",
        "`unsafe_` function can only be called from a `unsafe_` function.")
    generic_check(
        x,
        "_unsafe_QQQ(hole_variable_star)",
        "`unsafe_` function can only be called from a `unsafe_` function.")
end

