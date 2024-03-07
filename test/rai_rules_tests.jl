using StaticLint: StaticLint, run_lint_on_text, comp, convert_offset_to_line,
    convert_offset_to_line_from_lines, should_be_filtered, MarkdownFormat, PlainFormat
import CSTParser
using Test
using JSON3

# Reset the caches before running the tests.
StaticLint.reset_static_lint_caches()

function foo()
    @async 1 + 2
end

const n = Threads.nthreads()

function lint_test(source::String, expected_substring::String; verbose=true, directory::String = "")
    io = IOBuffer()
    run_lint_on_text(source; io, directory)
    output = String(take!(io))
    result = contains(output, expected_substring)
    verbose && !result && @warn "Not matching " output expected_substring
    return result
end

function lint_has_error_test(source::String, verbose=false)
    io = IOBuffer()
    run_lint_on_text(source; io=io)
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
        @test lint_test(source,
            "Line 14, column 1: `@generated` should be used with extreme caution.")
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
        @test lint_test(source, "Line 43, column 9: Use custom exception instead of the generic `error(...)`")

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

    @testset "in, equal, haskey" begin
        source = """
            function f()
                x = 10 in [10]
                y = in(10, [10])
                z = equal(10, "hello")
                w = haskey(Dict(1=>1000), 1)
            end
            """
        @test lint_has_error_test(source)
        @test lint_test(source,
            "Line 2, column 9: It is preferable to use `tin(item,collection)` instead of the Julia's `in`.")
        @test lint_test(source,
            "Line 3, column 9: It is preferable to use `tin(item,collection)` instead of the Julia's `in`.")
        @test lint_test(source,
            "Line 4, column 9: It is preferable to use `tequal(dict,key)` instead of the Julia's `equal`.")
        @test lint_test(source,
            "Line 5, column 9: It is preferable to use `thaskey(dict,key)` instead of the Julia's `haskey`.")
    end
end

@testset "Comparison" begin
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

    # in keyword
    @test t("in(hole_variable,hole_variable)", "in(x,y)")
    @test t("x in y", "hole_variable in hole_variable")
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
    @test convert_offset_to_line(43, source) == (2, 30, nothing)
    @test convert_offset_to_line(47, source) == (3, 4, nothing)

end

@testset "Should be filtered" begin
    filters = StaticLint.LintCodes[StaticLint.MissingReference, StaticLint.IncorrectCallArgs]
    hint_as_string1 = "Missing reference at offset 24104 of /Users/alexandrebergel/Documents/RAI/raicode11/src/DataExporter/export_csv.jl"
    hint_as_string2 = "Line 254, column 19: Possible method call error. at offset 8430 of /Users/alexandrebergel/Documents/RAI/raicode11/src/Compiler/Front/problems.jl"
    @test should_be_filtered(hint_as_string1, filters)
    @test !should_be_filtered(hint_as_string2, filters)

    @test should_be_filtered(hint_as_string1, filters)
    @test !should_be_filtered(hint_as_string2, filters)
end

@testset "Formatter" begin
    source = """
           const x = Threads.nthreads()
           function f()
               return x
           end
           """

    @testset "Plain 01" begin
        io = IOBuffer()
        run_lint_on_text(source; io=io, filters=StaticLint.no_filters)
        result = String(take!(io))

        expected = r"""
            ---------- \H+
            Line 1, column 11: `Threads.nthreads\(\)` should not be used in a constant variable\. at offset 10 of \H+
            Line 1, column 11: Missing reference at offset 10 of \H+
            2 potential threats are found
            ----------
            """
        @test !isnothing(match(expected, result))
    end

    @testset "Plain 02" begin
        io = IOBuffer()
        run_lint_on_text(source; io=io, filters=StaticLint.essential_filters)
        result = String(take!(io))

        expected = r"""
            ---------- \H+
            Line 1, column 11: `Threads.nthreads\(\)` should not be used in a constant variable\. at offset 10 of \H+
            1 potential threat is found
            ----------
            """
        @test !isnothing(match(expected, result))
    end

    @testset "Markdown 01" begin
        io = IOBuffer()
        run_lint_on_text(source; io=io, filters=StaticLint.no_filters, formatter=MarkdownFormat())
        result = String(take!(io))

        expected = r"""
             - \*\*Line 1, column 11:\*\* `Threads.nthreads\(\)` should not be used in a constant variable\. at offset 10 of \H+
             - \*\*Line 1, column 11:\*\* Missing reference at offset 10 of \H+
            """
        @test !isnothing(match(expected, result))
    end

    @testset "Markdown 02" begin
        io = IOBuffer()
        run_lint_on_text(source; io=io, filters=StaticLint.essential_filters, formatter=MarkdownFormat())
        result = String(take!(io))

        expected = r"""
             - \*\*Line 1, column 11:\*\* `Threads.nthreads\(\)` should not be used in a constant variable\. at offset 10 of \H+
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
             - \*\*\[Line 1, column 11:\]\(https://github\.com/RelationalAI/raicode/blob/axb-example-with-lint-errors/\H+/src/Compiler/tmp_julia_file\.jl#L1\)\*\* `Threads.nthreads\(\)` should not be used in a constant variable\. at offset 10 of \H+
            """
        println("DEBUG: $result")
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
             - \*\*\[Line 1, column 11:\]\(https://github\.com/RelationalAI/raicode/blob/axb-example-with-lint-errors/\H+/src/Compiler/tmp_julia_file\.jl#L1\)\*\* `Threads.nthreads\(\)` should not be used in a constant variable\. at offset 10 of \H+
            """
        @test !isnothing(match(expected, result))
    end
end

@testset "Run several times on same file" begin
    mktempdir() do dir
        open(joinpath(dir, "foo.jl"), "w") do io
            write(io, "function f()\n  @async 1 + 1\nend\n")
            flush(io)
            @test StaticLint.run_lint(dir; io) == 1
            @test StaticLint.run_lint(dir; io) == 1
            @test StaticLint.run_lint(dir; io) == 1
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
                         - \*\*Line 2, column 3:\*\* Macro `@spawn` should be used instead of `@async`\. at offset 15 of \H+
                         - \*\*Line 2, column 3:\*\* Macro `@spawn` should be used instead of `@async`\. at offset 15 of \H+
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
                    @test json_report[:data][:errors_count] == 3

                    local result
                    open(output_file) do oo
                        result = read(oo, String)
                    end

                    expected = r"""
                        ## Static code analyzer report
                        \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\)\*\*
                        Report creation time \(UTC\): \H+
                         - \*\*Line 2, column 3:\*\* Macro `@spawn` should be used instead of `@async`\. at offset 15 of \H+
                         - \*\*Line 2, column 3:\*\* `finalizer\(_,_\)` should not be used\. at offset 15 of \H+
                         - \*\*Line 2, column 25:\*\* Variable has been assigned but not used\. If you want to keep this variable unused then prefix it with `_`. at offset 37 of \H+
                        🚨\*\*In total, 3 errors are found over 2 Julia files\*\*🚨
                        """
                    result_matching = !isnothing(match(expected, result))
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
        @test json_report[:data][:errors_count] == 0


        expected = r"""
            ## Static code analyzer report
            \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\)\*\*
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
                @test json_report[:data][:errors_count] == 0

                local result
                open(output_file) do oo
                    result = read(oo, String)
                end


                expected = r"""
                    ## Static code analyzer report
                    \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\)\*\*
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
                    @test json_report[:data][:errors_count] == 0

                    local result
                    open(output_file) do oo
                        result = read(oo, String)
                    end

                    expected = r"""
                        ## Static code analyzer report
                        \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\)\*\*
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
                @test json_report[:data][:errors_count] == 0

                local result
                open(output_file) do oo
                    result = read(oo, String)
                end

                expected = r"""
                    ## Static code analyzer report
                    \*\*Output of the \[StaticLint\.jl code analyzer\]\(https://github\.com/RelationalAI/StaticLint\.jl\)\*\*
                    Report creation time \(UTC\): \H+
                    🎉No potential threats are found over 1 Julia file.👍
                    """
                result_matching = !isnothing(match(expected, result))
            end
        end
        @test result_matching
    end

    @testset "Report generation of 1 file with 1 error and github info" begin
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
                    file_prefix_to_remove="var/")

                json_report = JSON3.read(String(take!(json_io)))
                @test json_report[:source] == "StaticLint"
                @test json_report[:data][:files_count] == 1
                @test json_report[:data][:errors_count] == 1

                local result
                open(output_file) do oo
                    result = read(oo, String)
                end

                expected = r"""
                     - \*\*\[Line 2, column 3:\]\(https://github\.com/RelationalAI/raicode/blob/axb-foo-bar/folders/\H+/foo\.jl#L2\)\*\* Macro `@spawn` should be used instead of `@async`. at offset 15 of \H+
                    """
                result_matching = !isnothing(match(expected, result))
            end
        end
        @test result_matching
    end
end

@testset "Running on a directory" begin
    @testset "Non empty directory" begin
        local r = 0
        formatters = [StaticLint.PlainFormat(), StaticLint.MarkdownFormat()]
        for formatter in formatters
            mktempdir() do dir
                open(joinpath(dir, "foo.jl"), "w") do io
                    write(io, "function f()\n  @async 1 + 1\nend\n")
                    flush(io)
                    str = IOBuffer()
                    r += StaticLint.run_lint(dir; io=str, formatter)
                end
            end
        end
        @test r == 2
    end

    @testset "Empty directory" begin
        mktempdir() do dir
            @test iszero(StaticLint.run_lint(dir))
        end
    end
end

@testset "Locally disabling lint" begin
    @testset "lint-disable-lint" begin
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
        @test convert_offset_to_line_from_lines(46, source_lines) == (3, 4, Symbol("lint-disable-line"))
        @test convert_offset_to_line_from_lines(64, source_lines) == (5, 4, nothing)
    end
end

@testset "Relaxing unused bindings" begin
    @test lint_test("""
           function f(a::Int64, b, c)
               local x
               return 42
           end
           """, "Line 2, column 11: Variable has been assigned but not used.")

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