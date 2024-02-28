using Dates
using JSON3

function setup_server(
    env = dirname(SymbolServer.Pkg.Types.Context().env.project_file),
    depot = first(SymbolServer.Pkg.depots()),
    cache = joinpath(dirname(pathof(SymbolServer)), "..", "store")
)
    server = StaticLint.FileServer()
    ssi = SymbolServerInstance(depot, cache)
    _, symbols = SymbolServer.getstore(ssi, env)
    extended_methods = SymbolServer.collect_extended_methods(symbols)
    server.external_env = ExternalEnv(symbols, extended_methods, Symbol[])
    server
end

"""
    lint_string(s, server; gethints = false)

Parse a string and run a semantic pass over it. This will mark scopes, bindings,
references, and lint hints. An annotated `EXPR` is returned or, if `gethints = true`,
it is paired with a collected list of errors/hints.
"""
function lint_string(s::String, server = setup_server(); gethints = false)
    empty!(server.files)
    f = File("", s, CSTParser.parse(s, true), nothing, server)
    env = getenv(f, server)
    setroot(f, f)
    setfile(server, "", f)
    semantic_pass(f)
    check_all(f.cst, LintOptions(), env)
    if gethints
        hints = []
        for (offset, x) in collect_hints(f.cst, env)
            if haserror(x)
                push!(hints, (x, LintCodeDescriptions[x.meta.error]))
                push!(hints, (x, "Missing reference", " at offset ", offset))
            end
        end
        return f.cst, hints
    else
        return f.cst
    end
end

"""
    lint_file(rootpath, server)

Read a file from disc, parse and run a semantic pass over it. The file should be the
root of a project, e.g. for this package that file is `src/StaticLint.jl`. Other files
in the project will be loaded automatically (calls to `include` with complicated arguments
are not handled, see `followinclude` for details). A `FileServer` will be returned
containing the `File`s of the package.
"""
function lint_file(rootpath, server = setup_server(); gethints = false)
    empty!(server.files)
    root = loadfile(server, rootpath)
    semantic_pass(root)
    for f in values(server.files)
        check_all(f.cst, essential_options, getenv(f, server))
    end
    if gethints
        hints = []
        for (p,f) in server.files
            hints_for_file = []
            for (offset, x) in collect_hints(f.cst, getenv(f, server))
                if haserror(x)
                    if x.meta.error isa String
                        push!(hints_for_file, (x, string(x.meta.error, " at offset ", offset, " of ", p)))
                    else
                        push!(hints_for_file, (x, string(LintCodeDescriptions[x.meta.error], " at offset ", offset, " of ", p)))
                    end
                    push!(hints_for_file, (x, string("Missing reference", " at offset ", offset, " of ", p)))
                end
            end
            append!(hints, hints_for_file)
        end
        return root, hints
    else
        return root
    end
end

global global_server = setup_server()
const essential_options = LintOptions(true, false, true, true, true, true, true, true, true, false, true)

const no_filters = LintCodes[]
const essential_filters = [no_filters; [StaticLint.MissingReference, StaticLint.MissingFile]]


# Return (line, column) for a given offset in a source
function convert_offset_to_line_from_filename(offset::Int, filename::String)
    all_lines = open(io->readlines(io), filename)
    return convert_offset_to_line_from_lines(offset, all_lines)
end

function convert_offset_to_line(offset::Int, source::String)
    return convert_offset_to_line_from_lines(offset, split(source, "\n"))
end


function convert_offset_to_line_from_lines(offset::Int, all_lines)
    offset < 0 && throw(BoundsError("source", offset))

    current_index = 1
    annotation_previous_line = -1
    annotation = nothing
    for (index_line,line) in enumerate(all_lines)
        if endswith(line, "lint-disable-next-line")
            annotation_previous_line = index_line+1
        end

        if offset in current_index:(current_index + length(line))
            if endswith(line, "lint-disable-line") || (index_line == annotation_previous_line)
                annotation = Symbol("lint-disable-line")
            else
                annotation = nothing
            end
            result = index_line, (offset - current_index + 1), annotation
            annotation = nothing
            return result
        end
        current_index += length(line) + 1 #1 is for the Return line
    end

    throw(BoundsError("source", offset))
end

function should_be_filtered(hint_as_string::String, filters::Vector{LintCodes})
    return any(o->startswith(hint_as_string, LintCodeDescriptions[o]), filters)
end

abstract type AbstractFormatter end
struct PlainFormat <: AbstractFormatter end
struct MarkdownFormat <: AbstractFormatter end

"""
    filter_and_print_hint(hint, io::IO=stdout, filters::Vector=[])

Essential function to filter and print a `hint_as_string`, being a String.
Return true if the hint was printed, else it was filtered.
It takes the following arguments:
    - `hint_as_string` to be filtered or printed
    - `io` stream where the hint is printed, if not filtered
    - `filters` the set of filters to be used
"""
function filter_and_print_hint(hint_as_string::String, io::IO=stdout, filters::Vector{LintCodes}=LintCodes[], formatter::AbstractFormatter=PlainFormat())
    # Filter along the message
    should_be_filtered(hint_as_string, filters) && return false

    # Filter along the file content
    ss = split(hint_as_string)
    has_filename = isfile(last(ss))
    has_filename || error("Should have a filename")

    filename = string(last(ss))

    offset_as_string = ss[length(ss) - 2]
    # +1 is because CSTParser gives offset starting at 0.
    offset = Base.parse(Int64, offset_as_string) + 1

    line_number, column, annotation_line = convert_offset_to_line_from_filename(offset, filename)

    if isnothing(annotation_line)
        print_hint(formatter, io, "Line $(line_number), column $(column):", hint_as_string )
        return true
    end
    return false
end


function _run_lint_on_dir(
    rootpath::String;
    server = global_server,
    io::IO=stdout,
    filters::Vector{LintCodes}=essential_filters,
    formatter::AbstractFormatter=PlainFormat()
)
    local result = 0
    for (root, dirs, files) in walkdir(rootpath)
        for file in files
            filename = joinpath(root, file)
            if endswith(filename, ".jl")
                result += run_lint(filename; server, io, filters, formatter)
            end
        end

        for dir in dirs
            result += _run_lint_on_dir(joinpath(root, dir); server, io, filters, formatter)
        end
    end
    return result
end

function print_header(::PlainFormat, io::IO, rootpath::String)
    printstyled(io, "-" ^ 10 * " $(rootpath)\n", color=:blue)
end

function print_hint(::PlainFormat, io::IO, coordinates::String, hint::String)
    printstyled(io, coordinates, color=:green)
    print(io, " ")
    println(io, hint)
end

function print_summary(::PlainFormat, io::IO, nb_hints::Int)
    if iszero(nb_hints)
        printstyled(io, "No potential threats were found.\n", color=:green)
    else
        plural = nb_hints > 1 ? "s are" : " is"
        printstyled(io, "$(nb_hints) potential threat$(plural) found\n", color=:red)
    end
end

function print_footer(::PlainFormat, io::IO)
    printstyled(io, "-" ^ 10 * "\n\n", color=:blue)
end

function print_header(::MarkdownFormat, io::IO, rootpath::String)
    # println(io, "**Output of the [StaticLint.jl code analyzer]\
    #             (https://github.com/RelationalAI/StaticLint.jl) on file $(rootpath):**\n\
    #             UTC time: ($(now()))")
    return
end

print_footer(::MarkdownFormat, io::IO) = nothing
function print_hint(::MarkdownFormat, io::IO, coordinates::String, hint::String)
    print(io, " - **$coordinates** $hint\n")
end

function print_summary(::MarkdownFormat, io::IO, nb_hints::Int)
    # if iszero(nb_hints)
    #     print(io, "🎉No potential threats were found.👍\n\n")
    # else
    #     plural = nb_hints > 1 ? "s are" : " is"
    #     println(io, "🚨**$(nb_hints) potential threat$(plural) found**🚨\n")
    # end
    return
end

"""
    run_lint(rootpath::String; server = global_server, io::IO=stdout)

Run lint rules on a file `rootpath`, which must be an existing non-folder file. Return the
number of identifide errors.
Example of use:
    import StaticLint
    StaticLint.run_lint("foo/bar/myfile.jl")

"""
function run_lint(
    rootpath::String;
    server = global_server,
    io::IO=stdout,
    filters::Vector{LintCodes}=essential_filters,
    formatter::AbstractFormatter=PlainFormat()
)
    # If we are running Lint on a directory
    isdir(rootpath) && return _run_lint_on_dir(rootpath; server, io, filters, formatter)

    # Check if we have to be run on a Julia file. Simply exit if not.
    # This simplify the amount of work in GitHub Action
    endswith(rootpath, ".jl") || return 0

    # We are running Lint on a Julia file
    _,hints = StaticLint.lint_file(rootpath, server; gethints = true)

    print_header(formatter, io, rootpath)

    filtered_and_printed_hints = filter(h->filter_and_print_hint(h[2], io, filters, formatter), hints)
    number_of_error_found = length(filtered_and_printed_hints)
    print_summary(formatter, io, number_of_error_found)
    print_footer(formatter, io)
    return number_of_error_found
end

function run_lint_on_text(
    source::String;
    server = global_server,
    io::IO=stdout,
    filters::Vector{LintCodes}=essential_filters,
    formatter::AbstractFormatter=PlainFormat()
)
    tmp_file_name = tempname() * ".jl"
    open(tmp_file_name, "w") do file
        write(file, source)
        flush(file)
        run_lint(tmp_file_name; server, io, filters, formatter)
    end
end


"""
    generate_report(filenames::Vector{String}, output_filename::String)

Main entry point of StaticLint.jl. The function `generate_report` takes as argument a list
of files on which lint has to process. A report is generated containing the result.

The procuded markdown report is intenteded to be posted as a comment on a GitHub PR.
"""
function generate_report(filenames::Vector{String}, output_filename::String)
    if isfile(output_filename)
        @error "File $output_filename exist already."
        return
    end

    local errors_count = 0
    local files_count = length(filenames)

    open(output_filename, "w") do output_io
        println(output_io, "## Static code analyzer report")
        println(output_io, "**Output of the [StaticLint.jl code analyzer]\
            (https://github.com/RelationalAI/StaticLint.jl)**\n\
            Report creation time (UTC): ($(now()))")
        for filename in filenames
            errors_count += StaticLint.run_lint(
                                    filename;
                                    io=output_io,
                                    filters=essential_filters,
                                    formatter=MarkdownFormat())
        end

        has_julia_file = any(n->endswith(n, ".jl"), filenames)
        if !has_julia_file
            println(output_io, "No Julia file is modified or added in this PR.")
        else
            if iszero(errors_count)
                print(output_io, "🎉No potential threats are found over $(length(filenames)) files.👍\n\n")
            elseif errors_count == 1
                println(output_io, "🚨**In total, 1 error is found over $(length(filenames)) files**🚨")
            else
                println(output_io, "🚨**In total, $(errors_count) errors are found over $(files_count) files**🚨")
            end
        end
    end

    report_as_string = open(output_filename) do io read(io, String) end
    @info "StaticLint report" report_as_string errors_count files_count

    event = Dict(
        :id => uuid4(),
        :source => "StaticLint",
        :specversion => "1.0",
        :type => "result",
        :time => Dates.format(now(UTC), "yyyy-mm-ddTHH:MM:SSZ"), # RFC3339 format
        :data => Dict(:report_as_string=>report_as_string, :files_count=>files_count)
    )
    println(stdout, JSON3.write(event))
end