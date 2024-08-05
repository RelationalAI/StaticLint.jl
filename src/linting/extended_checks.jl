#################################################################################
# This file contains many specific and extended rules for Lint.
# You probably needs to modify this files if:
#   - you wish to add a new lint rule
#   - document an existing rule
#
# If you wish to add a new Lint rule, you need:
#   1. Define a new type, subtype of RecommendationExtendedRule or ViolationExtendedRule
#   2. Write a new function function check(t::YOUR_NEW_TYPE, x::EXPR)
#   3. Add your unit tests in rai_rules_tests.jl
#   4. Restart your REPL if you use it
#
# If you wish to modify the report produced by Lint, interface.jl
# is probably the place to start, not this file.
#################################################################################



#################################################################################
# UTILITY FUNCTIONS
#################################################################################
function is_named_hole_variable(x::CSTParser.EXPR)
    return x.head == :IDENTIFIER &&
            startswith(x.val, "hole_variable") &&
            x.val != "hole_variable_star" &&
            length(x.val) > length("hole_variable")
end

function is_hole_string(x::CSTParser.EXPR)
    return x.head == :STRING && startswith(x.val, "LINT_STRING")
end

function is_hole_variable(x::CSTParser.EXPR)
    return x.head == :IDENTIFIER && startswith(x.val, "hole_variable")
end

function is_hole_variable_star(x::CSTParser.EXPR)
    return x.head == :IDENTIFIER && x.val == "hole_variable_star"
end

comp(x, y) = x == y
raw_comp(x, y, named_variable_holes) = x == y

struct BothCannotHaveStarException <: Exception
    msg::String
end

comp_value(x, y) = x == y
function comp_value(x::String, y::String)
    is_there_any_star_marker = contains(x, "QQQ") || contains(y, "QQQ")
    !is_there_any_star_marker && return x == y

    contains(x, "QQQ") && contains(y, "QQQ") &&
        throw(BothCannotHaveStarException("Cannot both $x and $y have a star marker"))
    if contains(x, "QQQ")
        reg_exp = Regex(replace(x, "QQQ" => ".*"))
        return !isnothing(match(reg_exp, y))
    else
        reg_exp = Regex(replace(y, "QQQ" => ".*"))
        return !isnothing(match(reg_exp, x))
    end
end

function raw_comp(
    x::CSTParser.EXPR,
    y::CSTParser.EXPR,
    named_variable_holes::Vector
)
    # @info "debug:" x y
    # Main.infiltrate(@__MODULE__, Base.@locals, @__FILE__, @__LINE__)

    # If we bump into some named hole variables, then we record it.
    if is_named_hole_variable(x)
        push!(named_variable_holes, (x.val, y))
    end
    if is_named_hole_variable(y)
        push!(named_variable_holes, (y.val, x))
    end

    # If one of element to be compared is a hole, then we have a match!
    (is_hole_variable(x) || is_hole_variable(y)) && return true
    (is_hole_string(x) && y.head == :STRING) && return true
    (is_hole_string(y) && x.head == :STRING) && return true


    result = raw_comp(x.head, y.head, named_variable_holes) && comp_value(x.val, y.val)
    !result && return false

    min_length = min(length(x), length(y))

    for i in 1:min_length
        raw_comp(x[i], y[i], named_variable_holes) || return false
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

function comp(x::CSTParser.EXPR, y::CSTParser.EXPR)
    named_variable_holes = Vector()
    result = raw_comp(x, y, named_variable_holes)

    # If there is no or only one named variable hole, then we can exit
    length(named_variable_holes) <= 1 && return result

    all_hole_names = Set(first.(named_variable_holes))
    hole_names_to_values = Dict{String, CSTParser.EXPR}()
    # Else, we need to check that values under a unique named hole is the same
    for k in all_hole_names
        # Retrieve all the value for the named hole k
        relevant = filter(tp->first(tp) == k, named_variable_holes)
        relevant = map(tp->tp[2], relevant)

        # If there are more than 1 value for a given named hole k, then there is no match.
        first_relevant = relevant[1]
        all_others = relevant[2:end]
        all(r -> comp(first_relevant, r), all_others) || return false

        hole_names_to_values[k] = first_relevant
    end

    # Utility functions
    remove!(a, item) = deleteat!(a, findall(x->x==item, a))
    remove(a, item) = deleteat!(copy(a), findall(x->x==item, a))

    # At this point, we know that all the values for each named hole are the same.
    # We now need to check if values for each named holes are different.
    # If some values for two different named holes are the same, then there is no match
    nh_values = collect(values(hole_names_to_values))
    for v in nh_values
        all_to_check = remove(nh_values, v)
        any(k -> comp(k, v), all_to_check) && return false
    end
    return true
end

#################################################################################
# EXTENDED LINT RULES
#################################################################################
abstract type ExtendedRule end
abstract type RecommendationExtendedRule <: ExtendedRule end
abstract type ViolationExtendedRule <: ExtendedRule end

struct Async_Extention <: ViolationExtendedRule end
struct Ccall_Extention <: RecommendationExtendedRule end
struct Pointer_from_objref_Extention <: RecommendationExtendedRule end
struct NThreads_Extention <: ViolationExtendedRule end
struct Finalizer_Extention <: RecommendationExtendedRule end
struct CFunction_Extention <: RecommendationExtendedRule end
struct Semaphore_Extension <: RecommendationExtendedRule end
struct Destructor_Extension <: RecommendationExtendedRule end
struct ReentrantLock_Extension <: RecommendationExtendedRule end
struct SpinLock_Extension <: RecommendationExtendedRule end
struct Lock_Extension <: RecommendationExtendedRule end
struct Unlock_Extension <: RecommendationExtendedRule end
struct Yield_Extension <: RecommendationExtendedRule end
struct Sleep_Extension <: RecommendationExtendedRule end
struct Mmap_Extension <: RecommendationExtendedRule end
struct Future_Extension <: RecommendationExtendedRule end
struct Wait_Extension <: RecommendationExtendedRule end
struct Fetch_Extension <: RecommendationExtendedRule end
struct Inbounds_Extension <: RecommendationExtendedRule end
struct Atomic_Extension <: RecommendationExtendedRule end
struct Ptr_Extension <: RecommendationExtendedRule end
struct ArrayWithNoType_Extension <: ViolationExtendedRule end
struct Threads_Extension <: ViolationExtendedRule end
struct Generated_Extension <: RecommendationExtendedRule end
struct Sync_Extension <: RecommendationExtendedRule end
struct RemovePage_Extension <: ViolationExtendedRule end
struct Channel_Extension <: RecommendationExtendedRule end
struct Task_Extension <: ViolationExtendedRule end
struct ErrorException_Extension <: ViolationExtendedRule end
struct Error_Extension <: ViolationExtendedRule end
struct Unsafe_Extension <: ViolationExtendedRule end
struct In_Extension <: ViolationExtendedRule end
struct HasKey_Extension <: ViolationExtendedRule end
struct Equal_Extension <: ViolationExtendedRule end
struct Uv_Extension <: ViolationExtendedRule end
struct Splatting_Extension <: RecommendationExtendedRule end
struct UnreachableBranch_Extension <: ViolationExtendedRule end
struct StringInterpolation_Extension <: ViolationExtendedRule end
struct RelPathAPIUsage_Extension <: ViolationExtendedRule end
struct ReturnType_Extension <: ViolationExtendedRule end
struct NonFrontShapeAPIUsage_Extension <: ViolationExtendedRule end


const all_extended_rule_types = Ref{Any}(
    vcat(
        InteractiveUtils.subtypes(RecommendationExtendedRule),
        InteractiveUtils.subtypes(ViolationExtendedRule),
        )
)

# template -> EXPR to be compared
const check_cache = Dict{String, CSTParser.EXPR}()

# template -> error_msg
const error_msgs = Dict{String, String}()

function reset_recommentation_dict!(d::Dict{String, Bool})
    # Violations
    d["Variable has been assigned but not used, if you want to keep this variable unused then prefix it with `_`."] = false
    d[raw"Suspicious string interpolation, you may want to have $(a.b.c) instead of ($a.b.c)."] = false
end

function initialize_recommentation_dict()
    r = Dict{String, Bool}()
    reset_recommentation_dict!(r)
    return r
end

# msg -> is recommendation
const is_recommendation = initialize_recommentation_dict()

function reset_static_lint_caches()
    empty!(check_cache)
    empty!(error_msgs)
    reset_recommentation_dict!(is_recommendation)
    all_extended_rule_types[] = vcat(
        InteractiveUtils.subtypes(RecommendationExtendedRule),
        InteractiveUtils.subtypes(ViolationExtendedRule),
        )
    return nothing
end

function retrieve_full_msg_from_prefix(msg_prefix::String)
    the_keys = collect(keys(StaticLint.is_recommendation))
    is = findall(startswith(msg_prefix), the_keys)

    length(is) == 0 && return is

    if length(is) != 1
        isdefined(Main, :Infiltrator) && Main.infiltrate(@__MODULE__, Base.@locals, @__FILE__, @__LINE__)
    end
    @assert length(is) == 1
    return the_keys[first(is)]
end

function get_recommendation(msg_prefix)
    m = retrieve_full_msg_from_prefix(msg_prefix)
    m in keys(is_recommendation) || return nothing
    return is_recommendation[m]
end

function rule_is_recommendation(msg_prefix::String)
    r = get_recommendation(msg_prefix)
    isnothing(r) && return false
    return r
end

function rule_is_violation(msg_prefix::String)
    r = get_recommendation(msg_prefix)
    isnothing(r) && return false
    return !r
end

function get_oracle_ast(template_code::String)
    get!(check_cache, template_code, CSTParser.parse(template_code))
    return check_cache[template_code]
end

does_match(x::EXPR, template_code::String) = comp(x, get_oracle_ast(template_code))

function generic_check(t::ExtendedRule, x::EXPR, template_code::String, error_msg::String)
    generic_check(typeof(t), x, template_code, error_msg)
end

function generic_check(T::DataType, x::EXPR, template_code::String, error_msg::String)
    error_msg isa String && get!(error_msgs, template_code, error_msg)
    does_match(x, template_code) && seterror!(x, error_msg)
    check_for_recommendation(T, error_msg)
end

function generic_check(t::ExtendedRule, x::EXPR, template_code::String)
    generic_check(typeof(t), x, template_code)
end

function generic_check(T::DataType, x::EXPR, template_code::String)
    keyword = first(split(template_code, ['(', '{', ' ']))
    return generic_check(T, x, template_code, "`$(keyword)` should be used with extreme caution.")
end

function check_for_recommendation(T::DataType, msg::String)
    @assert supertype(T) in [RecommendationExtendedRule, ViolationExtendedRule]
    b = supertype(T) == RecommendationExtendedRule
    get!(is_recommendation, msg, b)
    return nothing
end

function check_with_process(T::DataType, x::EXPR, markers::Dict{Symbol,String})
    check(T(), x, markers)
end

# Useful for rules that do not need markers
check(t::Any, x::EXPR, markers::Dict{Symbol,String}) = check(t, x)

# The following function defines rules that are matched on the input Julia source code
# Each rule comes with a pattern that is checked against the abstract syntax tree
function check(t::Finalizer_Extention, x::EXPR)
    error_msg = "`finalizer(_,_)` should not be used."
    generic_check(t, x, "finalizer(hole_variable, hole_variable)", error_msg)
    generic_check(t, x, "finalizer(hole_variable) do hole_variable hole_variable_star end", error_msg)
end

function check(t::Async_Extention, x::EXPR)
    generic_check(t, x, "@async hole_variable", "Macro `@spawn` should be used instead of `@async`.")
    generic_check(t, x, "Threads.@async hole_variable", "Macro `@spawn` should be used instead of `@async`.")
end

check(t::Ccall_Extention, x::EXPR) = generic_check(t, x, "ccall(hole_variable, hole_variable, hole_variable, hole_variable_star)", "`ccall` should be used with extreme caution.")
check(t::Pointer_from_objref_Extention, x::EXPR) = generic_check(t, x, "pointer_from_objref(hole_variable)", "`pointer_from_objref` should be used with extreme caution.")

function check(t::NThreads_Extention, x::EXPR, markers::Dict{Symbol,String})
    # Threads.nthreads() must not be used in a const field, but it is allowed elsewhere
    haskey(markers, :const) || return
    generic_check(t, x, "Threads.nthreads()", "`Threads.nthreads()` should not be used in a constant variable.")
end

check(t::CFunction_Extention, x::EXPR) = generic_check(t, x, "@cfunction(hole_variable, hole_variable_star)", "Macro `@cfunction` should not be used.")
check(t::Semaphore_Extension, x::EXPR) = generic_check(t, x, "Semaphore(hole_variable)", "`Semaphore` should be used with extreme caution.")
check(t::ReentrantLock_Extension, x::EXPR) = generic_check(t, x, "ReentrantLock()", "`ReentrantLock` should be used with extreme caution.")

function check(t::Destructor_Extension, x::EXPR)
    error_msg = "Destructors should be used with extreme caution."
    generic_check(t, x, "destructor(hole_variable, hole_variable)", error_msg)
    generic_check(t, x, "destructor(hole_variable) do hole_variable hole_variable_star end", error_msg)
end

function check(t::SpinLock_Extension, x::EXPR)
    msg = "`SpinLock` should be used with extreme caution."
    generic_check(t, x, "SpinLock()", msg)
    generic_check(t, x, "Threads.SpinLock()", msg)
    generic_check(t, x, "Base.Threads.SpinLock()", msg)
end

function check(t::Lock_Extension, x::EXPR)
    msg = "`@lock` should be used with extreme caution."
    generic_check(t, x, "@lock hole_variable hole_variable", msg)
    generic_check(t, x, "Base.@lock hole_variable hole_variable", msg)
end

check(t::Unlock_Extension, x::EXPR) = generic_check(t, x, "unlock(hole_variable)")
check(t::Yield_Extension, x::EXPR) = generic_check(t, x, "yield()")
check(t::Sleep_Extension, x::EXPR) = generic_check(t, x, "sleep(hole_variable)")
function check(t::Mmap_Extension, x::EXPR)
    generic_check(t, x, "mmap(hole_variable_star)")
    generic_check(t, x, "Mmap.mmap(hole_variable_star)", "`mmap` should be used with extreme caution.")
end

check(t::Fetch_Extension, x::EXPR) = generic_check(t, x, "fetch(hole_variable)")
check(t::Inbounds_Extension, x::EXPR) = generic_check(t, x, "@inbounds hole_variable")

function check(t::Atomic_Extension, x::EXPR)
    msg = "`Atomic` should be used with extreme caution."
    generic_check(t, x, "Atomic(hole_variable_star)", msg)
    generic_check(t, x, "Atomic{hole_variable}(hole_variable_star)", msg)
    generic_check(t, x, "Threads.Atomic(hole_variable_star)", msg)
    generic_check(t, x, "Threads.Atomic{hole_variable}(hole_variable_star)", msg)
end

function check(t::Future_Extension, x::EXPR)
    generic_check(t, x, "Future{hole_variable}(hole_variable_star)")
    generic_check(t, x, "Future(hole_variable_star)")
end

check(t::Wait_Extension, x::EXPR) = generic_check(t, x, "wait(hole_variable)")
check(t::Ptr_Extension, x::EXPR) = generic_check(t, x, "Ptr{hole_variable}(hole_variable)")

function check(t::ArrayWithNoType_Extension, x::EXPR, markers::Dict{Symbol,String})
    haskey(markers, :filename) || return
    contains(markers[:filename], "src/Compiler") || return

    haskey(markers, :macrocall) && markers[:macrocall] == "@match" && return
    haskey(markers, :macrocall) && markers[:macrocall] == "@matchrule" && return

    generic_check(t, x, "[]", "Need a specific Array type to be provided.")
end

function check(t::Threads_Extension, x::EXPR)
    msg = "`@threads` should be used with extreme caution."
    generic_check(t, x, "Threads.@threads hole_variable", msg)
    generic_check(t, x, "@threads hole_variable", msg)
end

check(t::Generated_Extension, x::EXPR) = generic_check(t, x, "@generated hole_variable")

function check(t::Sync_Extension, x::EXPR)
    msg = "`@sync` should be used with extreme caution."
    generic_check(t, x, "@sync hole_variable", msg)
    generic_check(t, x, "Threads.@sync hole_variable", msg)
end

check(t::RemovePage_Extension, x::EXPR) = generic_check(t, x, "remove_page(hole_variable,hole_variable)")
check(t::Channel_Extension, x::EXPR) = generic_check(t, x, "Channel(hole_variable_star)")
check(t::Task_Extension, x::EXPR) = generic_check(t, x, "Task(hole_variable)")

function check(t::ErrorException_Extension, x::EXPR)
    generic_check(
        t,
        x,
        "ErrorException(hole_variable_star)",
        "Use custom exception instead of the generic `ErrorException`.")
end

function check(t::Error_Extension, x::EXPR)
    generic_check(
        t,
        x,
        "error(hole_variable)",
        "Use custom exception instead of the generic `error()`.")
end

function check(t::Unsafe_Extension, x::EXPR, markers::Dict{Symbol,String})
    haskey(markers, :function) || return
    isnothing(match(r"_unsafe_.*", markers[:function])) || return
    isnothing(match(r"unsafe_.*", markers[:function])) || return

    generic_check(
        t,
        x,
        "unsafe_QQQ(hole_variable_star)",
        "An `unsafe_` function should be called only from an `unsafe_` function.")
    generic_check(
        t,
        x,
        "_unsafe_QQQ(hole_variable_star)",
        "An `unsafe_` function should be called only from an `unsafe_` function.")
end

function check(t::In_Extension, x::EXPR)
    msg = "It is preferable to use `tin(item,collection)` instead of the Julia's `in` or `∈`."
    generic_check(t, x, "in(hole_variable,hole_variable)", msg)
    generic_check(t, x, "hole_variable in hole_variable", msg)

    generic_check(t, x, "∈(hole_variable,hole_variable)", msg)
    generic_check(t, x, "hole_variable ∈ hole_variable", msg)
end

function check(t::HasKey_Extension, x::EXPR)
    msg = "It is preferable to use `thaskey(dict,key)` instead of the Julia's `haskey`."
    generic_check(t, x, "haskey(hole_variable,hole_variable)", msg)
end

function check(t::Equal_Extension, x::EXPR)
    msg = "It is preferable to use `tequal(dict,key)` instead of the Julia's `equal`."
    generic_check(t, x, "equal(hole_variable,hole_variable)", msg)
end

function check(t::Uv_Extension, x::EXPR)
    generic_check(
        t,
        x,
        "uv_QQQ(hole_variable_star)",
        "`uv_` functions should be used with extreme caution.")
end

function check(t::Splatting_Extension, x::EXPR)
    generic_check(
        t,
        x,
        "hole_variable(hole_variable_star...)",
        "Splatting (`...`) should be used with extreme caution. Splatting from dynamically sized containers could result in severe performance degradation. Splatting from statically-sized tuples is usually okay. This lint rule cannot determine if this is dynamic or static, so please check carefully. See https://github.com/RelationalAI/RAIStyle#splatting for more information.")

    generic_check(
        t,
        x,
        "hole_variable([hole_variable(hole_variable_star) for hole_variable in hole_variable]...)",
        "Splatting (`...`) should not be used with dynamically sized containers. This may result in performance degradation. See https://github.com/RelationalAI/RAIStyle#splatting for more information.")
end

function check(t::UnreachableBranch_Extension, x::EXPR)
    generic_check(
        t,
        x,
        "if hole_variableA \
            hole_variable \
         elseif hole_variableA \
            hole_variable \
         end",
        "Unreachable branch.")
    generic_check(
        t,
        x,
        "if hole_variableA \
            hole_variable \
        elseif hole_variable \
            hole_variable\
        elseif hole_variableA \
            hole_variable \
        end",
        "Unreachable branch.")
end


# Argument is now a CSTParser string
# function check_string(x::EXPR)
#     @assert x.head == :string

#     msg_error = "Suspicious string interpolation."
#     # We iterate over the arguments of the CST String to check for STRING: (
#     # if we find one, this means the string was incorrectly interpolated
#     length(x.args) == 3 &&
#     x.args[1].head == :STRING && x.args[1].val == "(" &&
#     x.args[2].head == :IDENTIFIER &&
#     x.args[3].head == :STRING && !isnothing(match(r"^\.\H+", x.args[3].val)) &&
#     seterror!(x, msg_error)

# end


function check(t::StringInterpolation_Extension, x::EXPR)
    # We are interested only in string with interpolation, which begins with x.head==:string
    x.head == :string || return

    msg_error = raw"Suspicious string interpolation, you may want to have $(a.b.c) instead of ($a.b.c)."
    check_for_recommendation(typeof(t), msg_error)
    # We iterate over the arguments of the CST String to check for STRING: (
    # if we find one, this means the string was incorrectly interpolated

    for index in 1:(length(x.args)-1)
        x.args[index].head == :IDENTIFIER &&
        x.args[index+1].head == :STRING &&
        !isnothing(match(r"^\.[a-z,A-Z]+", x.args[index+1].val)) &&
        seterror!(x, msg_error)
    end
end

function check(t::RelPathAPIUsage_Extension, x::EXPR, markers::Dict{Symbol,String})
    haskey(markers, :filename) || return
    contains(markers[:filename], "src/Compiler/Front") || return

    generic_check(t, x, "hole_variable::RelPath", "Usage of type `RelPath` is not allowed in this context.")
    generic_check(t, x, "RelPath(hole_variable)", "Usage of type `RelPath` is not allowed in this context.")
    generic_check(t, x, "RelPath(hole_variable, hole_variable)", "Usage of type `RelPath` is not allowed in this context.")
    generic_check(t, x, "split_path(hole_variable)", "Usage of `RelPath` API method `split_path` is not allowed in this context.")
    generic_check(t, x, "drop_first(hole_variable)", "Usage of `RelPath` API method `drop_first` is not allowed in this context.")
    generic_check(t, x, "relpath_from_signature(hole_variable)", "Usage of method `relpath_from_signature` is not allowed in this context.")
end

function check(t::ReturnType_Extension, x::EXPR, markers::Dict{Symbol,String})
    # haskey(markers, :filename) || return
    # contains(markers[:filename], "src/Compiler/Front") || return

    generic_check(t, x, "hole_variable(hole_variable_star)::hole_variable = hole_variable", "Return type are prohibited.")
    generic_check(t, x, "function hole_variable(hole_variable_star)::hole_variable hole_variable_star end", "Return type are prohibited.")
end

function check(t::NonFrontShapeAPIUsage_Extension, x::EXPR, markers::Dict{Symbol,String})
    haskey(markers, :filename) || return
    # In the front-end and in FFI, we are allowed to refer to `Shape`
    contains(markers[:filename], "src/Compiler/Front") && return
    contains(markers[:filename], "src/Compiler/front2back.jl") && return
    contains(markers[:filename], "src/FFI") && return

    generic_check(t, x, "shape_term(hole_variable_star)", "Usage of `shape_term` Shape API method is not allowed outside of the Front-end Compiler and FFI.")
    generic_check(t, x, "Front.shape_term(hole_variable_star)", "Usage of `shape_term` Shape API method is not allowed outside of the Front-end Compiler and FFI.")
    generic_check(t, x, "shape_splat(hole_variable_star)", "Usage of `shape_splat` Shape API method is not allowed outside of the Front-end Compiler and FFI.")
    generic_check(t, x, "Front.shape_splat(hole_variable_star)", "Usage of `shape_splat` Shape API method is not allowed outside of the Front-end Compiler and FFI.")
    generic_check(t, x, "ffi_shape_term(hole_variable_star)", "Usage of `ffi_shape_term` is not allowed outside of the Front-end Compiler and FFI.")
    generic_check(t, x, "Shape", "Usage of `Shape` is not allowed outside of the Front-end Compiler and FFI.")
end
