using StaticLint: StaticLint, run_lint_on_text, comp, convert_offset_to_line,
    convert_offset_to_line_from_lines, should_be_filtered, MarkdownFormat, PlainFormat,
    fetch_value, rule_is_recommendation, rule_is_violation, has_values

using StaticLint: LintResult
import CSTParser
using Test
using JSON3

# Reset the caches before running the tests.
StaticLint.reset_static_lint_caches()

function lint_test(source::String, expected_substring::String; verbose=true, directory::String = "")
    io = IOBuffer()
    run_lint_on_text(source; io, directory)
    output = String(take!(io))
    result = contains(output, expected_substring)
    if verbose && !result
        printstyled("EXPECTED:\n$(expected_substring)\n\n", color=:green)
        printstyled("OUTPUT:\n$(output)\n\n", color=:red)
    end
    return result
end

function count_lint_errors(source::String, verbose=false; directory::String = "")
    io = IOBuffer()
    run_lint_on_text(source; io, directory)
    result = String(take!(io))
    all_lines = split(result, "\n")

    verbose && @info result
    # We remove decorations
    return length(filter(l->startswith(l, "Line "), all_lines))
end


function lint_has_error_test(source::String, verbose=false; directory::String = "")
    io = IOBuffer()
    run_lint_on_text(source; io, directory)
    result = String(take!(io))
    all_lines = split(result, "\n")

    verbose && @info result
    # We remove decorations
    return any(l->startswith(l, "Line "), all_lines)
end

# FOR FUTURE WORK
# @testset "string interpolation" begin
#     source = raw"""$(@async 1 + 2)"""
#     @test lint_has_error_test(source)
# end

@testset "forbidden macros" begin
    @testset "@async 01" begin
        source = """
            function f()
                @async 1 + 2
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 2, column 5: Macro `@spawn` should be used instead of `@async`.")
    end

    @testset "@async 02" begin
        source = """
            function f()
                Threads.@async 1 + 2
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 2, column 5: Macro `@spawn` should be used instead of `@async`.")
    end

    @testset "@cfunction" begin
        source = """
            function f()
                @cfunction(_readwrite_cb, Cvoid, (Ptr{Cvoid}, ))
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 2, column 5: Macro `@cfunction` should not be used.")
    end

    @testset "@lock, @threads, @generated" begin
        source = """
        function mark_transaction_as_database_creation!(kv::SpcsKV, transaction_id::String)
            @lock kv.latch begin
                push!(kv.create_database_transactions, transaction_id)
            end


            Threads.@threads for (e, v) in slots
                @test e[] == v
                free_slot!(pool, Blob{Nothing}(e))
            end
            return nothing
        end

        @generated function _empty_vector(::Type{T}) where {T}
            vec = T[]
            return vec
        end

        try
            @sync begin
                @spawn_pager_bg_task RAI_PagerCore.add_to_cache!(conf, page)
                @span_no_threshold "eot_rootmarker_write_to_blob" write_marker_to_cloud(
                    conf, page, rootinfo
                )
            end
        catch
            Threads.@sync begin
                1 + 2
            end
            nothing
        end
        """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 2, column 5: `@lock` should be used with extreme caution")
        @test lint_test(source,
            "Line 7, column 5: `@threads` should be used with extreme caution.")
        #@test lint_test(source,
        #    "Line 14, column 1: `@generated` should be used with extreme caution.")
        @test lint_test(source,
            "Line 20, column 5: `@sync` should be used with extreme caution.")
        @test lint_test(source,
            "Line 27, column 5: `@sync` should be used with extreme caution.")
    end
end

@testset "forbidden functions" begin
    @testset "nthreads() as a const" begin
        source = """
            const x = Threads.nthreads()
            function f()
                return x
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 1, column 11: `Threads.nthreads()` should not be used in a constant variable.")
    end

    @testset "nthreads() not as a const" begin
        source = """
            function f()
                return Threads.nthreads()
            end
            """
        @test !lint_has_error_test(source)
    end

    @testset "finalizer with do-end" begin
        source = """
            function f(x)
                ref = Ref(1)
                x = ___MutableFoo(ref)
                finalizer(x) do x
                    ref[] = 3
                end
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 4, column 5: `finalizer(_,_)` should not be used.")

    end

    @testset "finalizer without do-end" begin
        source = """
            function f(x)
                finalizer(q->nothing, x)
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 2, column 5: `finalizer(_,_)` should not be used.")
    end

    @testset "finalizer with do-end 02" begin
        source = """
            finalizer("hello") do x
                println("hello ")
                println("world")
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 1, column 1: `finalizer(_,_)` should not be used.")
    end

    @testset "destructor with do-end 02" begin
        source = """
            destructor("hello") do x
                println("hello ")
                println("world")
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 1, column 1: Destructors should be used with extreme caution")
    end

    @testset "ccall" begin
        source = """
            function rusage(who:: RUsageWho = RUSAGE_SELF)
                ru = Vector{RUsage}(undef, 1)
                ccall(:getrusage, Cint, (Cint, Ptr{Cvoid}), who, ru)
                return ru[1]
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 3, column 5: `ccall` should be used with extreme caution.")
    end

    @testset "ccall 02" begin
        source = """
            function _pread_async!(fd::Integer, buffer::Ptr{UInt8}, count::Integer, offset::Integer)::UInt64
                uv_filesystem_request, uv_buffer_descriptor = _prepare_libuv_async_call(buffer, count)

                ccall(:uv_fs_read, Int32,
                            (Ptr{Cvoid}, Ptr{Cvoid}, Int32, Ptr{Cvoid}, UInt32, Int64, Ptr{Cvoid}),
                            Base.eventloop(), uv_filesystem_request, fd, uv_buffer_descriptor, UInt32(1), offset,
                            @cfunction(_readwrite_cb, Cvoid, (Ptr{Cvoid}, ))
                            )
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 4, column 5: `ccall` should be used with extreme caution.")
    end

    @testset "pointer_from_objref 01" begin
        source = """
            function f(x)
                return pointer_from_objref(v)
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 2, column 12: `pointer_from_objref` should be used with extreme caution.")
    end

    @testset "pointer_from_objref 02" begin
        source = """
            function _reinterpret_with_size0(::Type{T1}, value::T2; checked::Bool=true) where {T1<:Tuple,T2<:Tuple}
                checked && _check_valid_reinterpret_with_size0(T1, T2)
                v = Ref(value)
                GC.@preserve v begin
                    ptr = pointer_from_objref(v)
                    return Base.unsafe_load(reinterpret(Ptr{T1}, ptr))
                end
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 5, column 15: `pointer_from_objref` should be used with extreme caution.")
    end

    @testset "pointer_from_objref 03" begin
        source = raw"""
            function vertex_name(c::Any)
                return "v$(UInt64(pointer_from_objref(c)))"
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 2, column 23: `pointer_from_objref` should be used with extreme caution.")
    end

    @testset "Semaphore" begin
        source = """
            const sem = Semaphore(5)
            function foo()
                return Semaphore(10)
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 1, column 13: `Semaphore` should be used with extreme caution.")
    end

    @testset "ReentrantLock" begin
        source = """
            const lock = ReentrantLock()
            function foo()
                lock2 = ReentrantLock()
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 3, column 13: `ReentrantLock` should be used with extreme caution.")
        @test lint_test(source,
            "Line 1, column 14: `ReentrantLock` should be used with extreme caution.")
    end

    @testset "SpinLock" begin
        source = """
            struct _SyncDict{Dict}
                lock::Base.Threads.SpinLock
                dict::Dict

                function _SyncDict{Dict}() where {Dict}
                    new{Dict}(Base.Threads.SpinLock(), Dict())
                end
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 6, column 19: `SpinLock` should be used with extreme caution.")
    end

    @testset "unlock" begin
        source = """
            function clear(fs::SimulatedFs)
                if fs.noop_mode
                    return nothing
                end
                for partition in fs.partitions
                    lock = trylock(partition.lock)
                    lock || error("SimFs partition locked on clear")
                    for (k, entry) in partition.entries
                        lock = trylock(entry.lock)
                        lock || error("SimFs entry locked on clear")
                        Blobs.free(entry.buf.data)
                        unlock(entry.lock)
                    end
                    empty!(partition.entries)
                    unlock(partition.lock)
                end
                @atomic fs.used_bytes = 0
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 15, column 9: `unlock` should be used with extreme caution.")
    end

    @testset "yield, sleep, map, Future, wait" begin
        source = """
            function wait_for_cooldown(count::UInt64, counts::HistogramCounts)
                while count != @atomic counts.total_observations
                    yield()
                    sleep(0.011)
                end
            end

            function use_of_map()
                s = open("/tmp/mmap.bin")
                m = read(s, Int)
                n = read(s, Int)
                A2 = mmap(s, Matrix{Int}, (m,n))
                A3 = Mmap.mmap(s, Matrix{Int}, (m,n))
                fut1 = Future{Any}() do f nothing end
                fut2 = Future{Nothing}(()->nothing)
                fut3 = Future(()->nothing)

                wait(TaskTreeJoin())
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 3, column 9: `yield` should be used with extreme caution.")
        @test lint_test(source,
            "Line 4, column 9: `sleep` should be used with extreme caution.")
        @test lint_test(source,
            "Line 12, column 10: `mmap` should be used with extreme caution.")
        @test lint_test(source,
            "Line 13, column 10: `mmap` should be used with extreme caution.")
        @test lint_test(source,
            "Line 14, column 12: `Future` should be used with extreme caution.")
        @test lint_test(source,
            "Line 15, column 12: `Future` should be used with extreme caution.")
        @test lint_test(source,
            "Line 16, column 12: `Future` should be used with extreme caution.")
        @test lint_test(source,
            "Line 18, column 5: `wait` should be used with extreme caution.")
    end

    @testset "fetch, @inbounds, Atomic, Ptr, remove_page, Channel, ErrorException" begin
        source = """
            function f()
                fut = Future{Any}()
                r1 = fetch(fut)

                @inbounds begin
                    at_end(iter) && return 0
                    i = 1
                    set_from_tuple!(columns_tuple, 1, iter_tuple(iter))
                    while next!(iter) && i < cap
                        i += 1
                        set_from_tuple!(columns_tuple, i, iter_tuple(iter))
                    end
                    return i
                end
                num_created1 = Threads.Atomic{Int}(0);
                num_created2 = Atomic{Int}(0);
                num_created3 = Atomic(0);

                pointer(page) == Ptr{Nothing}(0) && return
            end

            function _clear_pager!(pager)
                for (pid, _) in pager.owned_pages
                    remove_page(pager, pid)
                end
            end

            function foo()
                ch1 = Channel()
                ch2 = Channel(10)

                a() = sum(i for i in 1:1000);
                b = Task(a);

                e = ErrorException("failure")
                return (ch1, ch2)
            end

            function bar(x)
                throw(ErrorException("My error"))
            end

            bar() = error("My fault")
            """

        @test lint_test(source, "Line 3, column 10: `fetch` should be used with extreme caution.")
        @test lint_test(source, "Line 5, column 5: `@inbounds` should be used with extreme caution.")

        @test lint_test(source, "Line 15, column 20: `Atomic` should be used with extreme caution.")
        @test lint_test(source, "Line 16, column 20: `Atomic` should be used with extreme caution.")
        @test lint_test(source, "Line 17, column 20: `Atomic` should be used with extreme caution.")

        @test lint_test(source, "Line 19, column 22: `Ptr` should be used with extreme caution.")

        @test lint_test(source, "Line 24, column 9: `remove_page` should be used with extreme caution.")

        @test lint_test(source, "Line 29, column 11: `Channel` should be used with extreme caution.")
        @test lint_test(source, "Line 30, column 11: `Channel` should be used with extreme caution.")

        @test lint_test(source, "Line 33, column 9: `Task` should be used with extreme caution.")

        @test lint_test(source, "Line 40, column 11: Use custom exception instead of the generic `ErrorException`")
        @test lint_test(source, "Line 43, column 9: Use custom exception instead of the generic `error()`")

    end

    @testset "Array with no specific type 01" begin
        source = """
            function f()
                x = []
                y = String[]
                return vcat(x, y)
            end
            """
        # No error because this tmp file is not in the src/Compiler
        @test !lint_has_error_test(source)
    end

    @testset "Array with no specific type 02" begin
        source = """
            function f()
                x = []
                y = String[]
                return vcat(x, y)
            end
            """
        @test lint_test(
                source,
                "Line 2, column 9: Need a specific Array type to be provided.",
                directory = "src/Compiler/")
    end

    @testset "Array with no specific type 03" begin
        source = """
            function f()
                @matchrule bindings_empty() =
                    Bindings([], _::Missing) => CoreBindings([])

                @matchrule and_to_true() =
                    And([], annos) => BoolConstant(true, annos)

                @matchrule and_singleton() =
                    And([f], annos) => f

                @match CoreRelAbstract(bs2, [], as2) = e2

                @matchrule and_to_true() =
                    And([], annos) => BoolConstant(true, annos)

                @matchrule exists_empty() =
                    Exists(e, _) where is_definitely_empty_expr(e) =>
                        slice_to_false(input_val)
                f = []
            end
            """
        count_errors = count_lint_errors(source; directory = "src/Compiler/")
        @test count_errors == 1
        @test lint_test(
                source,
                "Line 19, column 9: Need a specific Array type to be provided.";
                directory = "src/Compiler")
    end

    @testset "in, equal, haskey, uv_" begin
        source = """
            function f()
                x = 10 in [10]
                y = in(10, [10])
                z = equal(10, "hello")
                w = haskey(Dict(1=>1000), 1)
                a = uv_foo(10, 20)
                b = ∈(10, [10])
                c = 10 ∈ [10]
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 2, column 9: It is preferable to use `tin(item,collection)` instead of the Julia's `in`")
        @test lint_test(source,
            "Line 3, column 9: It is preferable to use `tin(item,collection)` instead of the Julia's `in`")
        @test lint_test(source,
            "Line 4, column 9: It is preferable to use `tequal(dict,key)` instead of the Julia's `equal`.")
        @test lint_test(source,
            "Line 5, column 9: It is preferable to use `thaskey(dict,key)` instead of the Julia's `haskey`.")
        @test lint_test(source,
            "Line 6, column 9: `uv_` functions should be used with extreme caution.")
        @test lint_test(source,
            "Line 7, column 9: It is preferable to use `tin(item,collection)` instead of the Julia's `in` or `∈`.")
        @test lint_test(source,
            "Line 8, column 9: It is preferable to use `tin(item,collection)` instead of the Julia's `in` or `∈`.")
    end

    @testset "Splatting" begin
        @test lint_test("f(x...) + 10",
            "Line 1, column 1: Splatting (`...`) should be used with extreme caution. Splatting from dynamically sized containers could result in severe performance degradation.")

        @test lint_test("hcat([f(x) for x in r]...)",
            "Line 1, column 1: Splatting (`...`) should not be used with dynamically sized containers. This may result in performance degradation. See https://github.com/RelationalAI/RAIStyle#splatting for more information.")
    end
end

@testset "Comparing AST to templates" begin
    t(s1, s2) = comp(CSTParser.parse(s1), CSTParser.parse(s2))
    @test t("Threads.nthreads()", "Threads.nthreads()")
    @test !t("QWEThreads.nthreads()", "Threads.nthreads()")
    @test !t("Threads.nthreads()", "QWEThreads.nthreads()")
    @test !t("Threads.nthreads()", "Threads.qwenthreads()")

    @test t("1 + 2", "1+2")
    @test t("1 + 2", "1+hole_variable")
    @test t("hole_variable + hole_variable", "1 + hole_variable")
    @test t("hole_variable + 1", "1 + hole_variable")

    @test t("@async hole_variable", "@async begin 1 + 2 end")

    @test t("finalizer(y, x)", "finalizer(hole_variable, hole_variable)")
    @test t("finalizer(q->nothing, x)", "finalizer(hole_variable, hole_variable)")
    @test t("finalizer(x) do hole_variable hole_variable end",
            "finalizer(x) do x
                ref[] = 3
            end")

    @test !t("foo()", "foo(hole_variable)")
    @test !t("foo()", "foo(x)")

    @test t("foo(x, y)", "foo(hole_variable, hole_variable)")
    @test !t("foo(x, y, z)", "foo(hole_variable, hole_variable)")
    @test t("foo(x, y, z)", "foo(hole_variable, hole_variable_star)")

    @test t("foo(x, y, z)", "foo(hole_variable_star)")
    @test t("foo()", "foo(hole_variable_star)")

    @test t("Semaphore(10)", "Semaphore(hole_variable)")
    # Ideally, the next line should pass.
    # @test t("foo(x, y, z)", "foo(hole_variable, hole_variable, hole_variable, hole_variable_star)")

    source = """
            finalizer("hello") do x
                println("hello ")
            end
            """
    @test t(source, "finalizer(hole_variable) do hole_variable hole_variable_star end")

    @test t("unlock(hole_variable)", "unlock(12)")

    # Generics
    @test t("Future{Nothing}()", "Future{hole_variable}()")
    @test t("Future{T}()", "Future{hole_variable}(hole_variable_star)")
    @test t("Future{T}(()->nothing)", "Future{hole_variable}(hole_variable_star)")
    @test !t("Future()", "Future{hole_variable}(hole_variable_star)")
    @test !t("Future{Any}() do f nothing end", "Future{hole_variable}(hole_variable_star)")

    # Partial matching
    @test t("foo", "foo")
    @test t("fooQQQ", "foo")
    @test t("QQQfoo", "foo")
    @test t("QQQfooQQQ", "foo")

    @test !t("foo", "foobar")
    @test t("fooQQQ", "foobar")
    @test t("QQQfoo", "barfoo")
    @test t("QQQfooQQQ", "barfoozork")

    @test !t("foo", "foo_bar")
    @test t("fooQQQ", "foo_bar")
    @test t("QQQfoo", "bar_foo")
    @test t("QQQfooQQQ", "bar_foo_zork")

    @test t("foo(x, QQQzork)", "foo(x, zork)")
    @test t("foo(x, QQQzork)", "foo(x, blah_zork)")

    # in keyword
    @test t("in(hole_variable,hole_variable)", "in(x,y)")
    @test t("x in y", "hole_variable in hole_variable")

    # Splatting
    @test t("f(a...)", "hole_variable(hole_variable_star...)")
    @test t("hcat([f(x) for x in r]...)", "hole_variable([hole_variable(hole_variable_star) for hole_variable in hole_variable]...)")

    # Named variable holes
    @test t("1 + 2", "1+hole_variableA")
    @test t("1 + 2", "1+hole_variableB")
    @test t("1 + 2 + 2", "1+hole_variableA+hole_variableA")
    @test !t("1 + 2 + 2", "1+hole_variableA+hole_variableB")
    @test t("1 + 2 + 3", "1+hole_variableA+hole_variableB")
    @test t("1 + 2 + 3 + 2", "1+hole_variableA+hole_variable +hole_variableA")
    @test t("1 + 2 + 3 + 2 + 10", "1+hole_variableA+hole_variable +hole_variableA + hole_variable_star")

    @test !t("""
        if x == 1
            return 12
        elseif x== 2
            return "Reachable_branch"
        end
        """, """
        if hole_variableA == hole_variableB
            hole_variable
        elseif hole_variableA == hole_variableB
            hole_variable
        end
        """)

    @test t("""
        if x == 1
            return 12
        elseif x== 1
            return "Reachable_branch"
        end
        """, """
        if hole_variableA == hole_variableB
            hole_variable
        elseif hole_variableA == hole_variableB
            hole_variable
        end
        """)


    @test t("""
        if x == 1
            println("hello")
            return 12
        elseif x== 1
            println("world")
            return "Reachable_branch"
        end
        """, """
        if hole_variableA == hole_variableB
            hole_variable
        elseif hole_variableA == hole_variableB
            hole_variable
        end
        """)

    # LINT_STRING
    @test t("\"LINT_STRING\"", "\"this is a string\"")
    @test !t("\"LINT_STRING\"", "1 + 2")
    @test t("\"this is a string\"", "\"LINT_STRING\"")
    @test !t("1 + 2", "\"LINT_STRING\"")
    @test t("\"1 + 2\"", "\"LINT_STRING\"")

    # @test t(raw"\"($x)\"", "\"LINT_STRING\"")

end

@testset "unsafe functions" begin
    @testset "No error 01" begin
        source = """
            function unsafe_f()
                unsafe_g()
            end

            function unsafe_g()
                return 42
            end
            """
        @test !lint_has_error_test(source)
    end

    @testset "Some errors 01" begin
        source = """
            function f()
                unsafe_g()
            end

            function unsafe_g()
                return 42
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 2, column 5: An `unsafe_` function should be called only from an `unsafe_` function.")
    end

    @testset "Some errors 02" begin
        source = """
            function f()
                _unsafe_g()
            end

            function _unsafe_g()
                return 42
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 2, column 5: An `unsafe_` function should be called only from an `unsafe_` function.")
    end

    @testset "No error 02" begin
        source = """
            function f()
                # lint-disable-next-line
                _unsafe_g()
            end

            function _unsafe_g()
                return 42
            end
            """
        @test !lint_has_error_test(source)
    end
end

@testset "offset to line" begin
    source = """
        function f()
            return Threads.nthreads()
        end
        """
    @test_throws BoundsError convert_offset_to_line(-1, source)
    @test_throws BoundsError convert_offset_to_line(length(source) + 2, source)

    @test convert_offset_to_line(10, source) == (1, 10, nothing)
    @test convert_offset_to_line(20, source) == (2, 7, nothing)
    @test convert_offset_to_line(42, source) == (2, 29, nothing)
    @test convert_offset_to_line(46, source) == (3, 3, nothing)

end

@testset "Should be filtered" begin
    filters = StaticLint.LintCodes[StaticLint.MissingReference, StaticLint.IncorrectCallArgs]
    hint_as_string1 = "Missing reference. /Users/alexandrebergel/Documents/RAI/raicode11/src/DataExporter/export_csv.jl"
    hint_as_string2 = "Line 254, column 19: Possible method call error: foo /Users/alexandrebergel/Documents/RAI/raicode11/src/Compiler/Front/problems.jl"
    @test should_be_filtered(hint_as_string1, filters)
    @test !should_be_filtered(hint_as_string2, filters)

    @test should_be_filtered(hint_as_string1, filters)
    @test !should_be_filtered(hint_as_string2, filters)
end

@testset "Fetching values from AST" begin
    @test fetch_value(CSTParser.parse("f"), :IDENTIFIER) == "f"
    @test fetch_value(CSTParser.parse("f()"), :IDENTIFIER) == "f"
    @test fetch_value(CSTParser.parse("begin f(g()) end"), :IDENTIFIER) == "f"

    source = """
            struct _SyncDict{Dict}
                lock::Base.Threads.SpinLock
                dict::Dict

                function _SyncDict{Dict}() where {Dict}
                    new{Dict}(Base.Threads.SpinLock(), Dict())
                end
            end
            """
    @test fetch_value(CSTParser.parse(source), :IDENTIFIER) == "_SyncDict"

end

@testset "Formatter" begin
    source = """
           const x = Threads.nthreads()
           function f()
               return x
           end
           """

    # Rules that are not violation or recommandations are not processed.
    # @testset "Plain 01" begin
    #     io = IOBuffer()
    #     run_lint_on_text(source; io=io, filters=StaticLint.no_filters)
    #     result = String(take!(io))

    #     expected = r"""
    #         ---------- \H+
    #         Violations:
    #         Line 1, column 11: `Threads.nthreads\(\)` should not be used in a constant variable\. \H+
    #         Line 1, column 11: Missing reference \H+

    #         Recommendations:

    #         1 potential threat is found: 1 violation and 0 recommendation
    #         ----------
    #         """
    #     @test !isnothing(match(expected, result))
    # end

    @testset "Plain 02" begin
        io = IOBuffer()
        run_lint_on_text(source; io=io, filters=StaticLint.essential_filters)
        result = String(take!(io))

        expected = r"""
            ---------- \H+
            Line 1, column 11: `Threads.nthreads\(\)` should not be used in a constant variable\. \H+
            1 potential threat is found: 1 violation and 0 recommendation
            ----------
            """
        @test !isnothing(match(expected, result))
    end

    # Rules that are not violation or recommandations are not processed.
    # @testset "Markdown 01" begin
    #     io = IOBuffer()
    #     run_lint_on_text(source; io=io, filters=StaticLint.no_filters, formatter=MarkdownFormat())
    #     result = String(take!(io))

    #     expected = r"""
    #         Violations:
    #          - \*\*Line 1, column 11:\*\* `Threads.nthreads\(\)` should not be used in a constant variable\. \H+
    #          - \*\*Line 1, column 11:\*\* Missing reference. \H+

    #         Recommendations:

    #         """
    #         isdefined(Main, :Infiltrator) && Main.infiltrate(@__MODULE__, Base.@locals, @__FILE__, @__LINE__)
    #     @test !isnothing(match(expected, result))
    # end

    @testset "Markdown 02" begin
        io = IOBuffer()
        run_lint_on_text(source; io=io, filters=StaticLint.essential_filters, formatter=MarkdownFormat())
        result = String(take!(io))

        expected = r"""
             - \*\*Line 1, column 11:\*\* `Threads.nthreads\(\)` should not be used in a constant variable\. \H+
            """
        @test !isnothing(match(expected, result))
    end

    @testset "Markdown 03 - with github information" begin
        formatter = MarkdownFormat("axb-example-with-lint-errors", "RelationalAI/raicode")
        io = IOBuffer()

        run_lint_on_text(
            source;
            io,
            filters=StaticLint.essential_filters,
            formatter,
            directory="/src/Compiler/")
        result = String(take!(io))

        expected = r"""
             - \*\*\[Line 1, column 11:\]\(https://github\.com/RelationalAI/raicode/blob/axb-example-with-lint-errors/\H+/src/Compiler/tmp_julia_file\.jl#L1\)\*\* `Threads.nthreads\(\)` should not be used in a constant variable\. \H+
            """
        @test !isnothing(match(expected, result))
    end

    @testset "Markdown 04 - with github information" begin
        formatter = MarkdownFormat("axb-example-with-lint-errors", "RelationalAI/raicode")
        io = IOBuffer()
        run_lint_on_text(
            source;
            io,
            filters=StaticLint.essential_filters,
            formatter,
            directory="src/Compiler/")
        result = String(take!(io))
        expected = r"""
             - \*\*\[Line 1, column 11:\]\(https://github\.com/RelationalAI/raicode/blob/axb-example-with-lint-errors/\H+/src/Compiler/tmp_julia_file\.jl#L1\)\*\* `Threads.nthreads\(\)` should not be used in a constant variable\. \H+
            """
        @test !isnothing(match(expected, result))
    end
end

@testset "Run several times on same file" begin
    mktempdir() do dir
        open(joinpath(dir, "foo.jl"), "w") do io
            write(io, "function f()\n  @async 1 + 1\nend\n")
            flush(io)

            @test has_values(StaticLint.run_lint(dir; io), 1, 1, 0)
            @test has_values(StaticLint.run_lint(dir; io), 1, 1, 0)
            @test has_values(StaticLint.run_lint(dir; io), 1, 1, 0)
            @test has_values(StaticLint.run_lint(dir; io), 1, 1, 0)
        end
    end
end

@testset "Linting multiple files" begin
    @testset "No errors" begin
        local result_is_empty = false
        mktempdir() do dir
            open(joinpath(dir, "foo.jl"), "w") do io1
                open(joinpath(dir, "bar.jl"), "w") do io2
                    write(io1, "function f()\n  @spawn 1 + 1\nend\n")
                    write(io2, "function g()\n  @spawn 1 + 1\nend\n")

                    flush(io1)
                    flush(io2)

                    str = IOBuffer()
                    StaticLint.run_lint(dir; io=str, formatter=StaticLint.MarkdownFormat())
                    result = String(take!(str))
                    result_is_empty = isempty(result)
                end
            end
        end
        @test result_is_empty
    end

    @testset "Two files with errors" begin
        local result_matching = false
        mktempdir() do dir
            open(joinpath(dir, "foo.jl"), "w") do io1
                open(joinpath(dir, "bar.jl"), "w") do io2
                    write(io1, "function f()\n  @async 1 + 1\nend\n")
                    write(io2, "function g()\n  @async 1 + 1\nend\n")

                    flush(io1)
                    flush(io2)

                    str = IOBuffer()
                    StaticLint.run_lint(dir; io=str, formatter=StaticLint.MarkdownFormat())

                    result = String(take!(str))

                    expected = r"""
                         - \*\*Line 2, column 3:\*\* Macro `@spawn` should be used instead of `@async`\. \H+
                         - \*\*Line 2, column 3:\*\* Macro `@spawn` should be used instead of `@async`\. \H+
                        """
                    result_matching = !isnothing(match(expected, result))
                end
            end
        end
        @test result_matching
    end

    @testset "Report generation of two files with errors" begin
        local result_matching = false
        mktempdir() do dir
            file1 = joinpath(dir, "foo.jl")
            file2 = joinpath(dir, "bar.jl")
            open(file1, "w") do io1
                open(file2, "w") do io2
                    write(io1, "function f()\n  @async 1 + 1\nend\n")
                    write(io2, "function g()\n  finalizer(\"hello\") do x nothing\nend\n")

                    flush(io1)
                    flush(io2)

                    output_file = tempname()
                    json_io = IOBuffer()
                    StaticLint.generate_report([file1, file2], output_file; json_output=json_io)

                    json_report = JSON3.read(String(take!(json_io)))
                    @test json_report[:source] == "StaticLint"
                    @test json_report[:data][:files_count] == 2

                    @test json_report[:data][:violation_count] == 2
                    @test json_report[:data][:recommandation_count] == 1

                    local result
                    open(output_file) do oo
                        result = read(oo, String)
                    end

                    # First violations across files, then recommendations across files
                    expected = r"""
                        ## Static code analyzer report
                        \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\).+\*\*
                        Report creation time \(UTC\): \H+
                         - \*\*Line 2, column 3:\*\* Macro `@spawn` should be used instead of `@async`\. \H+
                         - \*\*Line 2, column 25:\*\* Variable has been assigned but not used, if you want to keep this variable unused then prefix it with `_`. \H+

                        <details>
                        <summary>For PR Reviewer \(1\)</summary>

                         - \*\*Line 2, column 3:\*\* `finalizer\(_,_\)` should not be used\. \H+\

                        </details>

                        🚨\*\*In total, 2 rule violations and 1 PR reviewer recommendation are found over 2 Julia files\*\*🚨
                        """
                    result_matching = !isnothing(match(expected, result))
                    # DEBUG:
                    !result_matching && @info result
                end
            end
        end
        @test result_matching
    end

    @testset "Report generation of two files with errors 02" begin
        local result_matching = false
        mktempdir() do dir
            file1 = joinpath(dir, "foo.jl")
            file2 = joinpath(dir, "bar.jl")
            open(file1, "w") do io1
                open(file2, "w") do io2
                    write(io1, "function g()\n  @async 1 + 1\nend\n  finalizer(\"hello\") do x nothing\nend\n")
                    write(io2, "function f()\n  @async 1 + 1\nend\n  finalizer(\"hello\") do x nothing\nend\n")

                    flush(io1)
                    flush(io2)

                    output_file = tempname()
                    json_io = IOBuffer()
                    StaticLint.generate_report([file1, file2], output_file; json_output=json_io)

                    json_report = JSON3.read(String(take!(json_io)))
                    @test json_report[:source] == "StaticLint"
                    @test json_report[:data][:files_count] == 2

                    @test json_report[:data][:recommandation_count] == 2
                    @test json_report[:data][:violation_count] == 2

                    local result
                    open(output_file) do oo
                        result = read(oo, String)
                    end

                    # First violations across files, then recommendations across files
                    expected = r"""
                        ## Static code analyzer report
                        \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\).+\*\*
                        Report creation time \(UTC\): \H+
                         - \*\*Line 2, column 3:\*\* Macro `@spawn` should be used instead of `@async`\. \H+
                         - \*\*Line 2, column 3:\*\* Macro `@spawn` should be used instead of `@async`\. \H+

                        <details>
                        <summary>For PR Reviewer \(2\)</summary>

                         - \*\*Line 4, column 3:\*\* `finalizer\(_,_\)` should not be used\. \H+
                         - \*\*Line 4, column 3:\*\* `finalizer\(_,_\)` should not be used\. \H+

                        </details>

                        🚨\*\*In total, 2 rule violations and 2 PR reviewer recommendations are found over 2 Julia files\*\*🚨
                        """
                    result_matching = !isnothing(match(expected, result))
                    # DEBUG:
                    !result_matching && @info result
                end
            end
        end
        @test result_matching
    end

    @testset "Report generation of two files with errors 02 - JSON report" begin
        local result_matching = false
        mktempdir() do dir
            file1 = joinpath(dir, "foo.jl")
            file2 = joinpath(dir, "bar.jl")
            open(file1, "w") do io1
                open(file2, "w") do io2
                    write(io1, "function g()\n  @async 1 + 1\nend\n  finalizer(\"hello\") do x nothing\nend\n")
                    write(io2, "function f()\n  @async 1 + 1\nend\n  finalizer(\"hello\") do x nothing\nend\n")

                    flush(io1)
                    flush(io2)

                    output_file = tempname()
                    json_filename = tempname()
                    @test !isfile(json_filename)
                    # json_io = IOBuffer()
                    StaticLint.generate_report([file1, file2], output_file; json_filename=json_filename)

                    @test isfile(json_filename)
                    json_content = open(io->read(io, String), json_filename)
                    json_report = JSON3.read(json_content)

                    @test json_report[:source] == "StaticLint"
                    @test json_report[:data][:files_count] == 2

                    @test json_report[:data][:recommandation_count] == 2
                    @test json_report[:data][:violation_count] == 2

                    local result
                    open(output_file) do oo
                        result = read(oo, String)
                    end

                    # First violations across files, then recommendations across files
                    expected = r"""
                        ## Static code analyzer report
                        \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\).+\*\*
                        Report creation time \(UTC\): \H+
                         - \*\*Line 2, column 3:\*\* Macro `@spawn` should be used instead of `@async`\. \H+
                         - \*\*Line 2, column 3:\*\* Macro `@spawn` should be used instead of `@async`\. \H+

                        <details>
                        <summary>For PR Reviewer \(2\)</summary>

                         - \*\*Line 4, column 3:\*\* `finalizer\(_,_\)` should not be used\. \H+
                         - \*\*Line 4, column 3:\*\* `finalizer\(_,_\)` should not be used\. \H+

                        </details>

                        🚨\*\*In total, 2 rule violations and 2 PR reviewer recommendations are found over 2 Julia files\*\*🚨
                        """
                    result_matching = !isnothing(match(expected, result))
                    # DEBUG:
                    !result_matching && @info result
                end
            end
        end
        @test result_matching
    end

    @testset "No modified julia file" begin
        output_file = tempname()
        json_io = IOBuffer()
        StaticLint.generate_report(String[], output_file; json_output=json_io)

        json_report = JSON3.read(String(take!(json_io)))
        @test json_report[:source] == "StaticLint"
        @test json_report[:data][:files_count] == 0

        @test json_report[:data][:recommandation_count] == 0
        @test json_report[:data][:violation_count] == 0


        expected = r"""
            ## Static code analyzer report
            \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\).+\*\*
            Report creation time \(UTC\): \H+
            No Julia file is modified or added in this PR.
            """
        result = open(io->read(io, String), output_file)

        result_matching = !isnothing(match(expected, result))
        @test result_matching
    end

    @testset "No result with no julia file" begin
        local result_matching = false
        mktempdir() do dir
            file1 = joinpath(dir, "foo.txt")
            open(file1, "w") do io1
                write(io1, "Hello World\n")
                flush(io1)

                output_file = tempname()
                json_io = IOBuffer()
                StaticLint.generate_report([file1], output_file; json_output=json_io)

                json_report = JSON3.read(String(take!(json_io)))
                @test json_report[:source] == "StaticLint"
                @test json_report[:data][:files_count] == 0
                @test json_report[:data][:recommandation_count] == 0
                @test json_report[:data][:violation_count] == 0

                local result
                open(output_file) do oo
                    result = read(oo, String)
                end


                expected = r"""
                    ## Static code analyzer report
                    \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\).+\*\*
                    Report creation time \(UTC\): \H+
                    No Julia file is modified or added in this PR.
                    """
                result_matching = !isnothing(match(expected, result))
            end
        end
        @test result_matching
    end

    @testset "Report generation of two files with no errors" begin
        local result_matching = false
        mktempdir() do dir
            file1 = joinpath(dir, "foo.jl")
            file2 = joinpath(dir, "bar.jl")
            open(file1, "w") do io1
                open(file2, "w") do io2
                    write(io1, "function f()\n  @spawn 1 + 1\nend\n")
                    write(io2, "function g()\n  @spawn 1 + 1\nend\n")

                    flush(io1)
                    flush(io2)

                    output_file = tempname()
                    json_io = IOBuffer()
                    StaticLint.generate_report([file1, file2], output_file; json_output=json_io)

                    json_report = JSON3.read(String(take!(json_io)))
                    @test json_report[:source] == "StaticLint"
                    @test json_report[:data][:files_count] == 2
                    @test json_report[:data][:recommandation_count] == 0
                    @test json_report[:data][:violation_count] == 0

                    local result
                    open(output_file) do oo
                        result = read(oo, String)
                    end

                    expected = r"""
                        ## Static code analyzer report
                        \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\).+\*\*
                        Report creation time \(UTC\): \H+
                        🎉No potential threats are found over 2 Julia files.👍
                        """
                    result_matching = !isnothing(match(expected, result))
                end
            end
        end
        @test result_matching
    end

    @testset "Report generation of 1 file with no errors" begin
        local result_matching = false
        mktempdir() do dir
            file1 = joinpath(dir, "foo.jl")
            open(file1, "w") do io1
                write(io1, "function f()\n  @spawn 1 + 1\nend\n")
                flush(io1)

                output_file = tempname()
                json_io = IOBuffer()
                StaticLint.generate_report([file1], output_file; json_output=json_io)

                json_report = JSON3.read(String(take!(json_io)))
                @test json_report[:source] == "StaticLint"
                @test json_report[:data][:files_count] == 1

                @test json_report[:data][:recommandation_count] == 0
                @test json_report[:data][:violation_count] == 0

                local result
                open(output_file) do oo
                    result = read(oo, String)
                end

                expected = r"""
                    ## Static code analyzer report
                    \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\).+\*\*
                    Report creation time \(UTC\): \H+
                    🎉No potential threats are found over 1 Julia file.👍
                    """
                result_matching = !isnothing(match(expected, result))
            end
        end
        @test result_matching
    end

    @testset "Report generation of 1 file with 1 error and github info" begin
        mktempdir() do dir
            file1 = joinpath(dir, "foo.jl")
            open(file1, "w") do io1
                write(io1, "function f()\n  @async 1 + 1\nend\n")
                flush(io1)

                output_file = tempname()
                json_io = IOBuffer()
                StaticLint.generate_report(
                    [file1],
                    output_file;
                    json_output=json_io,
                    github_repository="RelationalAI/raicode",
                    branch_name="axb-foo-bar",
                    file_prefix_to_remove="var/")

                json_report = JSON3.read(String(take!(json_io)))
                @test json_report[:source] == "StaticLint"
                @test json_report[:data][:files_count] == 1

                @test json_report[:data][:violation_count] == 1
                @test json_report[:data][:recommandation_count] == 0

                local result
                open(output_file) do oo
                    result = read(oo, String)
                end
                expected = "- **[Line 2, column 3:]" * 
                    "(https://github.com/RelationalAI/raicode/blob/axb-foo-bar$(dir)/foo.jl" *
                    "#L2)** Macro `@spawn` should be used instead of `@async`."
                if !occursin(expected, result)
                    @info "didn't match" expected result
                end
                @test occursin(expected, result)
            end
        end
    end

    @testset "Report generation of all the folder" begin
        # This is a slow test
        local result_matching = false
        mktempdir() do dir
            file1 = joinpath(dir, "foo.jl")
            open(file1, "w") do io1
                write(io1, "function f()\n  @async 1 + 1\nend\n")
                flush(io1)

                output_file = tempname()
                json_io = IOBuffer()
                StaticLint.generate_report(
                    [file1],
                    output_file;
                    json_output=json_io,
                    github_repository="RelationalAI/raicode",
                    branch_name="axb-foo-bar",
                    file_prefix_to_remove="var/",
                    analyze_all_file_found_locally=true
                )

                json_report = JSON3.read(String(take!(json_io)))
                @test json_report[:source] == "StaticLint"

                # There are more than 10 files in StaticLint.jl
                # and more than 1 violations and recommendations.
                @test json_report[:data][:files_count] > 10
                @test json_report[:data][:violation_count] > 1
                @test json_report[:data][:recommandation_count] > 0

                local result
                open(output_file) do oo
                    result = read(oo, String)
                end
                last_line = filter(!isempty, split(result, "\n"))[end]
                @test last_line != "No Julia file is modified or added in this PR."
            end
        end
    end

    @testset "Limiting report" begin
        # this tests create a Julia file with 100 violations, the report should mention
        # 100 violations, however only (an arbitrary) 30 are reported.
        local result_matching = false
        mktempdir() do dir
            file1 = joinpath(dir, "foo.jl")
            open(file1, "w") do io1
                write(io1, "function f()\n")
                for _ in 1:100
                    write(io1, "    @async 1 + 1\n")
                end
                write(io1, "end\n")
                flush(io1)

                output_file = tempname()
                json_io = IOBuffer()
                StaticLint.generate_report([file1], output_file; json_output=json_io)

                json_report = JSON3.read(String(take!(json_io)))
                @test json_report[:source] == "StaticLint"
                @test json_report[:data][:files_count] == 1
                @test json_report[:data][:recommandation_count] == 0
                @test json_report[:data][:violation_count] == 100

                local result
                open(output_file) do oo
                    result = read(oo, String)
                end
                all_lines = split(result, "\n")
                lines_count = length(all_lines)
                @test lines_count < 70

                @test all_lines[end-2] == "⚠️Only a subset of the violations and recommandations are here reported⚠️"
                @test all_lines[end-1] == "🚨**In total, 100 rule violations and 0 PR reviewer recommendation are found over 1 Julia file**🚨"
                @test all_lines[end] == ""
            end
        end
    end
end

@testset "Running on a directory" begin
    @testset "Non empty directory" begin
        local r
        r = LintResult()

        formatters = [StaticLint.PlainFormat(), StaticLint.MarkdownFormat()]
        for formatter in formatters
            mktempdir() do dir
                open(joinpath(dir, "foo.jl"), "w") do io
                    write(io, "function f()\n  @async 1 + 1\nend\n")
                    flush(io)
                    str = IOBuffer()
                    append!(r, StaticLint.run_lint(dir; io=str, formatter))
                end
            end
        end
        @test (r.violations_count + r.recommendations_count) == 2
    end

    @testset "Empty directory" begin
        mktempdir() do dir
            @test StaticLint.run_lint(dir) == LintResult()
        end
    end
end

@testset "Locally disabling lint" begin
    @testset "lint-disable-lint" begin
        @test convert_offset_to_line(17, """
            function f()
                @async 1 + 2 # lint-disable-line
            end
            """) == (2, 4, "lint-disable-line")
        @test !lint_has_error_test("""
            function f()
                @async 1 + 2 # lint-disable-line
            end
            """)
        @test !lint_has_error_test("""
            function f()
                @async 1 + 2 #lint-disable-line
            end
            """)

        @test !lint_has_error_test("""
            function f()
                @async 1 + 2 #  lint-disable-line
            end
            """)

        @test lint_has_error_test("""
            function f()
                @async 1 + 2 #  lint-disable-line
                @async 1 + 3
            end
            """)
    end
    @testset "lint-disable-next-line" begin
        @test !lint_has_error_test("""
            function f()
                # lint-disable-next-line
                @async 1 + 2
            end
            """)
        @test !lint_has_error_test("""
            function f()
                # lint-disable-next-line
                @async 1 + 2
            end
            """)

        @test !lint_has_error_test("""
            function f()
                # lint-disable-next-line
                @async 1 + 2
            end
            """)

        @test lint_has_error_test("""
            function f()
                # lint-disable-next-line
                @async 1 + 2

                @async 1 + 3
            end
            """)
        @test lint_has_error_test("""
            function f()
                @async 1 + 2
                # lint-disable-next-line

                @async 1 + 3
            end
            """)
        @test lint_has_error_test("""
            function f()
                @async 1 + 2
                # lint-disable-next-line
                @async 1 + 3
            end
            """)

        source = """
            function f()
                # lint-disable-next-line
                @async 1 + 2

                @async 1 + 3
            end
            """
        source_lines = split(source, "\n")
        @test convert_offset_to_line_from_lines(46, source_lines) == (3, 4, "lint-disable-line")
        @test convert_offset_to_line_from_lines(64, source_lines) == (5, 4, nothing)
    end

    @testset "Locally disabling rule 01" begin
        source = """
        function f()
            # lint-disable-next-line: Macro `@spawn` should be used instead of `@async`.
            @async 1 + 1
        end
        """
        source_lines = split(source, "\n")
        @test convert_offset_to_line_from_lines(30, source_lines) == (2, 17, nothing)
        @test convert_offset_to_line_from_lines(95, source_lines) == (3, 1, "lint-disable-line: Macro `@spawn` should be used instead of `@async`.")

        @test !lint_has_error_test(source)
    end

    @testset "Locally disabling rule 02" begin
        source = """
        function f()
            # lint-disable-next-line: Macro `@spawn` should be used instead of `@async`.
            @async unsafe_foo(12)
        end
        """
        source_lines = split(source, "\n")
        @test convert_offset_to_line_from_lines(30, source_lines) == (2, 17, nothing)
        @test convert_offset_to_line_from_lines(95, source_lines) == (3, 1, "lint-disable-line: Macro `@spawn` should be used instead of `@async`.")

        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 3, column 12: An `unsafe_` function should be called only from an `unsafe_` function.")
    end

    @testset "Locally disabling rule 03" begin
        source = """
        function f()
            # lint-disable-next-line: An `unsafe_` function
            @async unsafe_foo(42)
        end
        """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 3, column 5: Macro `@spawn` should be used instead of `@async`.")
    end

    @testset "Locally disabling rule 04" begin
        source = """
        function f()
            # lint-disable-next-line:Macro `@spawn` should be used instead of `@async`.
            @async 1 + 1
        end
        """
        source_lines = split(source, "\n")
        @test convert_offset_to_line_from_lines(30, source_lines) == (2, 17, nothing)
        @test convert_offset_to_line_from_lines(95, source_lines) == (3, 2, "lint-disable-line: Macro `@spawn` should be used instead of `@async`.")

        @test !lint_has_error_test(source)
    end

    @testset "Locally disabling rule 05" begin
        source = """
        function f()
            # lint-disable-next-line:  Macro `@spawn` should be used instead of `@async`.
            @async 1 + 1
        end
        """
        source_lines = split(source, "\n")
        @test convert_offset_to_line_from_lines(30, source_lines) == (2, 17, nothing)
        @test convert_offset_to_line_from_lines(97, source_lines) == (3, 2, "lint-disable-line: Macro `@spawn` should be used instead of `@async`.")

        @test !lint_has_error_test(source)
    end
end

@testset "Relaxing unused bindings" begin
    @test lint_test("""
           function f(a::Int64, b, c)
               local x
               return 42
           end
           """, "Line 2, column 11: Variable has been assigned but not used")

    @test !lint_has_error_test("""
           function f(a::Int64, b, c)
               local _
               return 42
           end
           """)

    @test !lint_has_error_test("""
           function f(a::Int64, b, c)
               local _x
               return 42
           end
           """)
end

# This rule is disabled per default. To add it, need to modify reset_recommentation_dict!()
# @testset "IncorrectCallArgs" begin
#     source = """
#                f(x) = 1
#                f(1, 2)
#                """
#     @test lint_test(source, "Line 2, column 1: Possible method call error: f.")
# end

@testset "Branch" begin
    @testset "Reachable branches" begin
        source = """
            function f(x)
                if x == 1
                    return 12
                elseif x== 2
                    return "Reachable_branch"
                end
            end
            """
        @test !lint_has_error_test(source)
    end

    @testset "Unreachable branches" begin
        source = """
            function f(x)
                if x == 1
                    return 12
                elseif x== 1
                    return "Unreachable_branch"
                end
            end
            """
        @test lint_test(source, "Line 2, column 5: Unreachable branch.")
    end

    @testset "Unreachable branches 02" begin
        source = """
            function f(x)
                if x <= 1
                    return 12
                elseif x <= 1
                    return "Unreachable_branch"
                end
            end
            """
        @test lint_test(source, "Line 2, column 5: Unreachable branch.")
    end
end

@testset "Resetting StaticLint caches" begin
    source = """
    function f()
        @async 1 + 1
    end
    function g()
        @lock Lock() begin
            1 + 1
        end
    end
    """

    run_lint_on_text(source; io=IOBuffer())

    @test !isempty(StaticLint.check_cache)
    @test !isempty(StaticLint.error_msgs)
    @test !isempty(StaticLint.is_recommendation)

    StaticLint.reset_static_lint_caches()

    @test isempty(StaticLint.check_cache)
    @test isempty(StaticLint.error_msgs)

    # is_recommendation should contains Lint default rules that we want to consider
    @test !isempty(StaticLint.is_recommendation)
end

@testset "Recommandation vs violation" begin
    source = """
    function f()
        @async 1 + 1
    end
    function g()
        @lock Lock() begin
            1 + 1
        end
    end
    """
    io=IOBuffer()
    run_lint_on_text(source; io)

    @test !rule_is_recommendation("Macro `@spawn` should be used instead of `@async`.")
    @test !rule_is_recommendation("Macro `@spawn`")
    @test  rule_is_violation("Macro `@spawn` should be used instead of `@async`.")
    @test  rule_is_violation("Macro `@spawn`")
    @test  rule_is_recommendation("`@lock` should be used with extreme caution.")
    @test  rule_is_recommendation("`@lock` ")
    @test !rule_is_violation("`@lock` should be used with extreme caution.")
    @test !rule_is_violation("`@lock` ")

    @test  rule_is_recommendation("Splatting (`...`) should be used with extreme caution.")


    @test StaticLint.retrieve_full_msg_from_prefix("`@lock` ") ==
        "`@lock` should be used with extreme caution."
    @test StaticLint.retrieve_full_msg_from_prefix("Macro `@spawn`") ==
        "Macro `@spawn` should be used instead of `@async`."

    msg = "Macro `@spawn` should be used instead of `@async`."
    @test StaticLint.retrieve_full_msg_from_prefix(msg) == msg
end

@testset "Recommandation vs violation - Default error msg" begin
    @test rule_is_violation("Variable has been assigned but not used")
    @test rule_is_violation("Variable has been assigned but not used, if you want to keep this variable unused then prefix it with `_`.")
    @test !rule_is_recommendation("Variable has been assigned but not used")
end

@testset "Recommentation separated from violations" begin
    source = """
    function f()
        @async 1 + 1
    end
    function g()
        @lock Lock() begin
            1 + 1
        end
    end
    """
    io=IOBuffer()
    run_lint_on_text(source; io)

    result = String(take!(io))
    expected = r"""
    ---------- \H+
    Line 2, column 5: Macro `@spawn` should be used instead of `@async`\. \H+
    Line 5, column 5: `@lock` should be used with extreme caution\. \H+
    2 potential threats are found: 1 violation and 1 recommendation
    ----------
    """
    @test !isnothing(match(expected, result))
end

@testset "Checking string interpolation" begin
    source_with_error = raw"""
    function f(conf)
        @info "($conf.container.baseurl)"
    end
    """

    source_with_error2 = raw"""
    function f(conf)
        @info "$conf.container.baseurl"
    end
    """

    source_with_error3 = raw"""
    function f(conf)
        @info "this string contains an error $conf.container.baseurl indeed!"
    end
    """

    source_without_error = raw"""
    function f(conf)
        @info "$(conf.container.baseurl)"
    end
    """

    source_without_error2 = raw"""
    function f(conf)
        @info "this string contains an error $(conf.container.baseurl) indeed!"
    end
    """

    source_without_error3 = raw"""
    function f(conf)
        @info "this string contains an error $conf .container.baseurl indeed!"
    end
    """

    source_without_error4 = raw"""
    function f(engine_name)
        @info "Issuing delete request for engine $engine_name..."
    end
    """
    @test lint_test(source_with_error, raw"Line 2, column 11: Suspicious string interpolation, you may want to have $(a.b.c) instead of ($a.b.c).")
    @test lint_test(source_with_error2, raw"Line 2, column 11: Suspicious string interpolation, you may want to have $(a.b.c) instead of ($a.b.c).")
    @test lint_test(source_with_error3, raw"Line 2, column 11: Suspicious string interpolation, you may want to have $(a.b.c) instead of ($a.b.c).")

    @test count_lint_errors(source_without_error) == 0
    @test count_lint_errors(source_without_error2) == 0
    @test count_lint_errors(source_without_error3) == 0
    @test count_lint_errors(source_without_error4) == 0


    # FALSE POSITIVE
    # StaticLint reports false positive, ie., a rule violation that is not an issue
    false_positive1 = raw"""
    function f()
        profile_filename = "profile-$(timestamp).pb.gz"
    end
    """

    false_positive2 = raw"""
    function f()
        Source("model/$name", "model/$name",  read(joinpath(@__DIR__, "models", "$name.rel"), String))
    end
    """
    false_positive3 = raw"""
    function f()
        path = "$dir/$name.csv"
    end
    """

    @test lint_test(false_positive1, raw"Line 2, column 24: Suspicious string interpolation, you may want to have $(a.b.c) instead of ($a.b.c).")
    @test lint_test(false_positive2, raw"Line 2, column 77: Suspicious string interpolation, you may want to have $(a.b.c) instead of ($a.b.c).")
    @test lint_test(false_positive3, raw"Line 2, column 12: Suspicious string interpolation, you may want to have $(a.b.c) instead of ($a.b.c).")

end

@testset "Arithmetic LintResult" begin
    l1 = LintResult()
    l2 = LintResult(1, 2, 3)
    l3 = LintResult(10, 20, 30)
    l4 = LintResult(10, 20, 30, ["foo.jl"], 100)
    l5 = LintResult(10, 20, 30, ["foo2.jl"], 250)

    @test l1 == l1
    @test l1 == LintResult()
    # @test (l1 + l2) == l2
    # @test (l3 + l2) == LintResult(11, 22, 33)
    @test l4 != l5
    @test l3 != l4
    @test l3 != l5

    append!(l4, l5)
    @test l4 == LintResult(20, 40, 60, ["foo.jl", "foo2.jl"], 350)
end

@testset "RelPath Front-End" begin
    source = """
        function rel_sig_from_relpath(path)
            (name, types) = split_path(path)
            return RelationSignature(name, types.elements)
        end

        function interpret(x, y, path)
            rest = drop_first(path)
            return RelPath(rest.elements[2:end])
        end

        function reverse(decl::EdbDecl)
            return relpath_from_signature(decl.signature)
        end

        function use_path(x, y::RelPath, z)
            return y.elements
        end
    """

    @test count_lint_errors(source; directory="/src/Compiler/Front") == 5
    @test count_lint_errors(source; directory="") == 0

    @test lint_test(source,
        "Line 2, column 25: Usage of `RelPath` API method `split_path` is not allowed in this context.";
        directory="/src/Compiler/Front"
    )
end
