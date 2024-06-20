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
    markers::Dict{Symbol,String} = Dict(:filename => rootpath)
    for f in values(server.files)
        check_all(f.cst, essential_options, getenv(f, server), markers)
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
const essential_filters = [no_filters; [StaticLint.MissingReference, StaticLint.MissingFile, StaticLint.InvalidTypeDeclaration]]

# Return (line, column) for a given offset in a source
function convert_offset_to_line_from_filename(offset::Union{Int64, Int32}, filename::String)
    all_lines = open(io->readlines(io), filename)
    return convert_offset_to_line_from_lines(offset, all_lines)
end

function convert_offset_to_line(offset::Int, source::String)
    return convert_offset_to_line_from_lines(offset, split(source, "\n"))
end


# Return a triple: (index_line, index_column, annotation)
# annotation could be either nothing, or :lint-disable-next-line
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

# MarkdownFormat can optionally contains github information. This is useful when a
# report is generated which contains Markdown links.
# file_prefix_to_remove corresponds to a prefix files will be removed when generating the report
struct MarkdownFormat <: AbstractFormatter
    github_branch_name::String
    github_repository_name::String
    file_prefix_to_remove::String
    MarkdownFormat() = new("", "", "")
    MarkdownFormat(branch::String, repo::String, prefix::String) = new(branch, repo, prefix)
    MarkdownFormat(branch::String, repo::String) = new(branch, repo, "")
end

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

    # Remove the offset from the result. No need for this.
    cleaned_hint = replace(hint_as_string, (" at offset $offset_as_string of" => ""))
    try
        line_number, column, annotation_line = convert_offset_to_line_from_filename(offset, filename)

        # isdefined(Man, :Infiltrator) && Main.infiltrate(@__MODULE__, Base.@locals, @__FILE__, @__LINE__)
        local error_type
        cleaned_hint_without_filename = cleaned_hint[1:end-length(filename)-1]
        if !(cleaned_hint_without_filename in keys(error_msg2type))
            error_type = string(StaticLint.error_msg2type[cleaned_hint_without_filename])
        else
            error_type = nothing
        end

        function typestring_from_annotation(annotation::String)
            isnothing(annotation) && return nothing
            contains(annotation, ":") || return nothing
            return split(annotation, ":")[2]
        end
        if isnothing(annotation_line) || typestring_from_annotation(annotation_line) !== error_type
            print_hint(formatter, io, "Line $(line_number), column $(column):", cleaned_hint)
            return true
        end
    catch e
        # isdefined(Main, :Infiltrator) && Main.infiltrate(@__MODULE__, Base.@locals, @__FILE__, @__LINE__)
        @error "Cannot retreive offset=$offset in file $filename error=$e"
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

print_header(::MarkdownFormat, io::IO, rootpath::String) = nothing
print_footer(::MarkdownFormat, io::IO) = nothing

function print_hint(format::MarkdownFormat, io::IO, coordinates::String, hint::String)
    if !isempty(format.github_branch_name) && !isempty(format.github_repository_name)
        line_number = split(coordinates, [' ', ','])[2]
        file_name = last(split(hint, " "))
        corrected_file_name = first(file_name) == '/' ? file_name[2:end] : file_name
        if startswith(corrected_file_name, format.file_prefix_to_remove)
            corrected_file_name = corrected_file_name[length(format.file_prefix_to_remove)+1:end]
        end
        extended_coordinates = "[$coordinates](https://github.com/$(format.github_repository_name)/blob/$(format.github_branch_name)/$(corrected_file_name)#L$(line_number))"
        print(io, " - **$extended_coordinates** $hint\n")
    else
        print(io, " - **$coordinates** $hint\n")
    end
end

print_summary(::MarkdownFormat, io::IO, nb_hints::Int) = nothing

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

"""
file_name corresponds to a file name that is used to create the temporary file. This is
useful to test some rules that depends on the filename.

`directory` can be "src/Compiler". In that case, the file to be created is "tmp_julia_file.jl"
"""
function run_lint_on_text(
    source::String;
    server = global_server,
    io::IO=stdout,
    filters::Vector{LintCodes}=essential_filters,
    formatter::AbstractFormatter=PlainFormat(),
    directory::String = ""   # temporary directory to be created. If empty, let Julia decide
)
    local tmp_file_name, tmp_dir
    local correct_directory = ""
    if isempty(directory)
        tmp_file_name = tempname() * ".jl"
    else
        correct_directory = first(directory) == '/' ? directory[2:end] : directory
        tmp_dir = joinpath(tempdir(), correct_directory)
        mkpath(tmp_dir)
        tmp_file_name = joinpath(tmp_dir, "tmp_julia_file.jl")
    end
    open(tmp_file_name, "w") do file
        write(file, source)
        flush(file)
        run_lint(tmp_file_name; server, io, filters, formatter)
    end

    # If a directory has been provided, then it needs to be deleted, after manually deleting the file
    if !isempty(correct_directory)
        rm(tmp_file_name)
        rm(tmp_dir)
    end
end

function print_datadog_report(
    json_output::IO,
    report_as_string::String,
    files_count::Int64,
    errors_count::Int64
)
    event = Dict(
        :source => "StaticLint",
        :specversion => "1.0",
        :type => "result",
        :time => string(now(UTC)), #Dates.format(now(UTC), "yyyy-mm-ddTHH:MM:SSZ"), # RFC3339 format
        :data => Dict(
                    :report_as_string=>report_as_string,
                    :files_count => files_count,
                    :errors_count => errors_count)
    )
    println(json_output, JSON3.write(event))
end

"""
    generate_report(filenames::Vector{String}, output_filename::String, json_output::IO=stdout)

Main entry point of StaticLint.jl. The function `generate_report` takes as argument a list
of files on which lint has to process. A report is generated containing the result.

The procuded markdown report is intenteded to be posted as a comment on a GitHub PR.

When provided, `github_repository` and `branch_name` are used to have clickable linkgs in
the the Markdown report.
"""
function generate_report(
    filenames::Vector{String},
    output_filename::String;
    json_output::IO=stdout,
    github_repository::String="",
    branch_name::String="",
    file_prefix_to_remove::String=""
)
    if isfile(output_filename)
        @error "File $output_filename exist already."
        return
    end

    local errors_count = 0
    local julia_filenames = filter(n->endswith(n, ".jl"), filenames)
    local files_count = length(julia_filenames)

    open(output_filename, "w") do output_io
        println(output_io, "## Static code analyzer report")
        println(output_io, "**Output of the [StaticLint.jl code analyzer]\
            (https://github.com/RelationalAI/StaticLint.jl)**\n\
            Report creation time (UTC): ($(now()))")

        formatter=MarkdownFormat(branch_name, github_repository, file_prefix_to_remove)
        for filename in julia_filenames
            errors_count += StaticLint.run_lint(
                                    filename;
                                    io=output_io,
                                    filters=essential_filters,
                                    formatter)
        end

        has_julia_file = any(n->endswith(n, ".jl"), julia_filenames)
        ending = length(julia_filenames) > 1 ? "s" : ""
        if !has_julia_file
            println(output_io, "No Julia file is modified or added in this PR.")
        else
            if iszero(errors_count)
                print(output_io, "🎉No potential threats are found over $(length(julia_filenames)) Julia file$(ending).👍\n\n")
            elseif errors_count == 1
                println(output_io, "🚨**In total, 1 potential threat is found over $(length(julia_filenames)) Julia file$(ending)**🚨")
            else
                println(output_io, "🚨**In total, $(errors_count) potential threats are found over $(files_count) Julia file$(ending)**🚨")
            end
        end
    end

    report_as_string = open(output_filename) do io read(io, String) end
    print_datadog_report(json_output, report_as_string, files_count, errors_count)
end