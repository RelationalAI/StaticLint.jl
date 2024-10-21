using Dates
using JSON3

global MAX_REPORTED_ERRORS = 60 # 1_000_000

# Global result of executing Lint on files and folders
mutable struct LintResult
    files_count::Integer
    violations_count::Integer
    recommendations_count::Integer
    linted_files::Vector{String}
    printout_count::Integer
    has_fatal_hint::Bool

    LintResult(a, b, c, d, e, f) = new(a, b, c, d, e, f)
end

# Each individual rule violation report
mutable struct LintRuleReport
    rule::ExtendedRule
    msg::String
    template::String
    file::String
    line::Int64
    column::Int64
    is_disabled::Bool   # Happens with a comments in the code

    offset::Int64
end
LintRuleReport(rule::ExtendedRule, msg::String) = LintRuleReport(rule, msg, "", "", 0, 0, false, 0)

LintResult() = LintResult(0, 0, 0, String[], 0, false)
LintResult(a, b, c) = LintResult(a, b, c, String[])
LintResult(a, b, c, d) = LintResult(a, b, c, d, 0, false)
LintResult(a, b, c, d, e) = LintResult(a, b, c, d, e, false)

# function Base.:+(l1::LintResult, l2::LintResult)
#     return LintResult(
#         l1.files_count + l2.files_count,
#         l1.violations_count + l2.violations_count,
#         l1.recommendations_count + l2.recommendations_count,
#         vcat(l1.linted_files, l2.linted_files)
#     )
# end

function Base.append!(l1::LintResult, l2::LintResult)
    # if !isempty(l2.linted_files)
    #     if first(l2.linted_files) in l1.linted_files
    #         isdefined(Main, :Infiltrator) && Main.infiltrate(@__MODULE__, Base.@locals, @__FILE__, @__LINE__)
    #         @error "ERROROOOOO"
    #     end
    # end

    l1.files_count += l2.files_count
    l1.violations_count += l2.violations_count
    l1.recommendations_count += l2.recommendations_count
    Base.append!(l1.linted_files, l2.linted_files)
    l1.printout_count += l2.printout_count
end

function Base.:(==)(l1::LintResult, l2::LintResult)
    return l1.files_count == l2.files_count &&
           l1.violations_count == l2.violations_count &&
           l1.recommendations_count == l2.recommendations_count &&
           l1.linted_files == l2.linted_files &&
           l1.printout_count == l2.printout_count
end

function is_already_linted(l::LintResult, filename)
    return filename in l.linted_files
end

function has_values(l::LintResult, a, b, c)
    return  l.files_count == a &&
            l.violations_count == b &&
            l.recommendations_count == c
end

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
                push!(hints, (x, "Missing reference.", " at offset ", offset))
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
    lint_rule_reports = []
    if gethints
        hints = []
        for (p,f) in server.files
            hints_for_file = []
            for (offset, x) in collect_hints(f.cst, getenv(f, server))
                if haserror(x)
                    # TODO: On some point, we should only have the LintRuleReport case
                    if x.meta.error isa String
                        push!(hints_for_file, (x, string(x.meta.error, " at offset ", offset, " of ", p)))
                    elseif x.meta.error isa LintRuleReport
                        # The next line should be deleted
                        push!(hints_for_file, (x, string(x.meta.error.msg, " at offset ", offset, " of ", p)))

                        lint_rule_report = x.meta.error
                        lint_rule_report.offset = offset

                        line_number, column, annotation_line = convert_offset_to_line_from_filename(lint_rule_report.offset + 1, lint_rule_report.file)
                        lint_rule_report.line = line_number
                        lint_rule_report.column = column

                        # If the annotation is to disable lint,
                        if annotation_line == "lint-disable-line"
                            # then we disable it.
                        elseif !isnothing(annotation_line) && startswith("lint-disable-line: $(lint_rule_report.msg)", annotation_line)
                            # then we disable it.
                        else
                            # Else we record it.
                            push!(lint_rule_reports, lint_rule_report)
                        end
                    else
                        push!(hints_for_file, (x, string(LintCodeDescriptions[x.meta.error], " at offset ", offset, " of ", p)))

                        isdefined(Main, :Infiltrator) && Main.infiltrate(@__MODULE__, Base.@locals, @__FILE__, @__LINE__)
                    end
                    push!(hints_for_file, (x, string("Missing reference.", " at offset ", offset, " of ", p)))
                end
            end
            append!(hints, hints_for_file)
        end
        return root, hints, lint_rule_reports
    else
        return root
    end
end

global global_server = nothing
const essential_options = LintOptions(true, false, true, true, true, true, true, true, true, false, true)

const no_filters = LintCodes[]
const essential_filters = [no_filters; [StaticLint.MissingReference, StaticLint.MissingFile, StaticLint.InvalidTypeDeclaration]]

# Return (index_line, index_column, annotation) for a given offset in a source
function convert_offset_to_line_from_filename(offset::Union{Int64, Int32}, filename::String)
    all_lines = open(io->readlines(io), filename)
    return convert_offset_to_line_from_lines(offset, all_lines)
end

function convert_offset_to_line(offset::Integer, source::String)
    return convert_offset_to_line_from_lines(offset, split(source, "\n"))
end

# Return the lint next-line annotation, if there is one, at the end of `line`.
# Return
#   * `nothing`      if there is no `lint-disable-next-line` annotation.
#   * ""::SubString  if the end of the line is "lint-disable-next-line".
#   * s::SubString   if the end of the line is "lint-disable_next_line: $s"
function annotation_for_next_line(line::AbstractString)
    if endswith(line, "lint-disable-next-line")
        return ""
    end
    # An annotation must be in a comment and not contain any `#` or `"` characters.
    m = match(r"# lint-disable-next-line: *([^\"#]+)$", line)
    return isnothing(m) ? nothing : m[1]
end

function annotation_for_this_line(line::AbstractString)
    if endswith(line, "lint-disable-line")
        return ""
    end
    # An annotation must be in a comment and not contain any `#` or `"` characters.
    m = match(r"#\h*lint-disable-line: *([^\"#]+)$", line)
    return isnothing(m) ? nothing : m[1]
end

# Return a triple: (line::Int, column::Int, annotation::Option(String))
#
# `annotation` could be either `nothing`, "lint-disable-line", or
# `"lint-disable-line: $ERROR_MSG_TO_IGNORE"`
#
# Note: `offset` is measured in codepoints.  The returned `column` is a character
# offset, not a codepoint offset.
function convert_offset_to_line_from_lines(offset::Integer, all_lines)
    offset < 0 && throw(BoundsError("source", offset))

    current_codepoint = 1
    # In these annotations, "" means "lint-disable-line", a nonempty string `s` means
    # "lint_disable_line: $s", and nothing means there's no applicable annotation.
    prev_annotation::Union{Nothing,SubString} = nothing
    this_annotation::Union{Nothing,SubString} = nothing
    for (line_number, line) in enumerate(all_lines)
        this_annotation = annotation_for_this_line(line)
        # current_codepoint + sizeof(line) is possibly pointing at the newline that isn't
        # actually stored in `line`.
        if offset in current_codepoint:(current_codepoint + sizeof(line))
            index_in_line = offset - current_codepoint + 1 # possibly off the end by 1.
            if !isnothing(this_annotation)
                annotation = this_annotation
            elseif !isnothing(prev_annotation)
                annotation = prev_annotation
            else
                annotation = nothing
            end
            if !isnothing(annotation)
                if annotation == ""
                    annotation = "lint-disable-line"
                else
                    annotation = "lint-disable-line: " * annotation
                end
            end
            if index_in_line == sizeof(line) + 1
                return line_number, length(line)+1, annotation
            else
                return line_number, length(line, 1, index_in_line), annotation
            end
        end
        prev_annotation = annotation_for_next_line(line)
        current_codepoint += sizeof(line) + 1 # 1 is for the newline
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
# file_prefix_to_remove corresponds to a prefix files will be removed when generating the
# report. This is useful because GitHub Action clones a repository in a folder of the same
# name. In our case, GHA will create /home/runner/work/raicode/raicode so we need to remove
# one "raicode" from the fullname.
struct MarkdownFormat <: AbstractFormatter
    github_branch_name::String
    github_repository_name::String
    file_prefix_to_remove::String
    stream_workflowcommand::IO

    MarkdownFormat() = new("", "", "", devnull)
    MarkdownFormat(
        branch::String,
        repo::String,
        prefix::String,
        stream_workflowcommand::IO) = new(branch, repo, prefix, stream_workflowcommand)
    MarkdownFormat(branch::String, repo::String) = new(branch, repo, "", devnull)
end

# Being fatal means the lint process will exit(1) when run from pre-commit
function is_fatal(hint::String)
    patterns = ["Unsafe", "@safe"]
    return any(p->contains(hint, p), patterns)
end

should_print_report(result) = result.printout_count <= MAX_REPORTED_ERRORS


"""
@@TODO: THIS FUNCTION MUST BE REMOVED, LEGACY

    filter_and_print_hint(...)

Essential function to filter and print a `hint_as_string`, being a String.
Return true if the hint was printed, else it was filtered.
It takes the following arguments:
    - `hint_as_string` to be filtered or printed
    - `io` stream where the hint is printed, if not filtered
    - `filters` the set of filters to be used
    - `lint_result` is the current lint result. We used it to limit the produced report.
"""
function filter_and_print_hint(
    hint_as_string::String,
    lint_result::LintResult,
    io::IO=stdout,
    filters::Vector{LintCodes}=LintCodes[],
    formatter::AbstractFormatter=PlainFormat(),
)
    # If fatal, then indicate this in the result
    if is_fatal(hint_as_string)
        lint_result.has_fatal_hint = true
    end

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
    cleaned_hint = replace(hint_as_string, (" at offset $(offset_as_string) of" => ""))

    should_print_hint(result) = result.printout_count <= MAX_REPORTED_ERRORS
    try
        line_number, column, annotation_line = convert_offset_to_line_from_filename(offset, filename)

        has_no_annotation = isnothing(annotation_line)
        if has_no_annotation
            # No annotation, so we merely print the reported error.
            if should_print_hint(lint_result)
                print_hint(formatter, io, "Line $(line_number), column $(column):", cleaned_hint)
                lint_result.printout_count += 1
            end
            return true
        else
            # there is an annotation, we need to distinguish if it is specific or not
            is_generic_disable_annotation = annotation_line == "lint-disable-line"
            if is_generic_disable_annotation
                return false
            end

            v = match(r"lint-disable-line: (?<msg>.*)$", annotation_line)
            msg = isnothing(v) ? nothing : v[:msg]

            # if it is specific, and the reported error is different from the provided error
            # then we report the error
            if !isnothing(msg) && startswith(cleaned_hint, msg)
                return false
            end

            if should_print_hint(lint_result)
                print_hint(formatter, io, "Line $(line_number), column $(column):", cleaned_hint)
                lint_result.printout_count += 1
            end
            return true
        end
    catch e
        @assert e isa BoundsError
        @error "Cannot retrieve offset=$(offset) in file $(filename)"
    end
    return false
end


function _run_lint_on_dir(
    rootpath::String;
    result::LintResult=LintResult(),
    server = global_server,
    io::Union{IO,Nothing}=stdout,
    io_violations::Union{IO,Nothing}=nothing,
    io_recommendations::Union{IO,Nothing}=nothing,
    filters::Vector{LintCodes}=essential_filters,
    formatter::AbstractFormatter=PlainFormat()
)
    # Exit if we are in .git
    !isnothing(match(r".*/\.git.*", rootpath)) && return result

    for (root, dirs, files) in walkdir(rootpath)
        for file in files
            filename = joinpath(root, file)
            if endswith(filename, ".jl")
                run_lint(filename; result, server, io, io_violations, io_recommendations, filters, formatter)
            end
        end

        for dir in dirs
            p = joinpath(root, dir)
            !isnothing(match(r".*/\.git.*", p)) && continue
            _run_lint_on_dir(p; result, server, io, io_violations, io_recommendations, filters, formatter)
        end
    end
    return result
end

function print_header(::PlainFormat, io::IO, rootpath::String)
    printstyled(io, "-" ^ 10 * " $(rootpath)\n", color=:blue)
end

# TODO: REMOVE THIS LEGACY CODE
function print_hint(::PlainFormat, io::IO, coordinates::String, hint::String)
    printstyled(io, coordinates, color=:green)
    print(io, " ")
    println(io, hint)
end

function print_report(::PlainFormat, io::IO, lint_report::LintRuleReport)
    printstyled(io, "Line $(lint_report.line), column $(lint_report.column):", color=:green)
    print(io, " ")
    print(io, lint_report.msg)
    print(io, " ")
    println(io, lint_report.file)
end

function print_summary(
    ::PlainFormat,
    io::IO,
    count_violations::Integer,
    count_recommendations::Integer
)
    nb_hints = count_violations + count_recommendations
    if iszero(nb_hints)
        printstyled(io, "No potential threats were found.\n", color=:green)
    else
        plural = nb_hints > 1 ? "s are" : " is"
        plural_vio = count_violations > 1 ? "s" : ""
        plural_rec = count_recommendations > 1 ? "s" : ""
        printstyled(io, "$(nb_hints) potential threat$(plural) found: ", color=:red)
        printstyled(io, "$(count_violations) violation$(plural_vio) and $(count_recommendations) recommendation$(plural_rec)\n", color=:red)
    end
end

function print_footer(::PlainFormat, io::IO)
    printstyled(io, "-" ^ 10 * "\n\n", color=:blue)
end

print_header(::MarkdownFormat, io::IO, rootpath::String) = nothing
print_footer(::MarkdownFormat, io::IO) = nothing

# Remove a leading '/' if the file starts with one. This is necessary to build the URL
# Remove the prefix mentioned in the Markdown from the file_name
function remove_prefix_from_filename(file_name::String, file_prefix_to_remove::String)
    corrected_file_name = first(file_name) == '/' ? file_name[2:end] : file_name
    if startswith(corrected_file_name, file_prefix_to_remove)
        corrected_file_name = corrected_file_name[length(file_prefix_to_remove)+1:end]
    end
    return corrected_file_name
end

function remove_prefix_from_filename(file_name::String, format::MarkdownFormat)
    return remove_prefix_from_filename(file_name, format.file_prefix_to_remove)
end

# Essential function to print a lint report using the Markdown
# coordinates can be "Line 6, column 44:"
# TODO: LEGACY CODE, REMOVE IT!!!!
function print_hint(format::MarkdownFormat, io::IO, coordinates::String, hint::String)
    coord = split(coordinates, [' ', ',', ':'])
    column_number = coord[5]
    line_number = coord[2]
    file_name = string(last(split(hint, " ")))
    corrected_file_name = remove_prefix_from_filename(file_name, format)

    if !isempty(format.github_branch_name) && !isempty(format.github_repository_name)
        extended_coordinates = "[$(coordinates)](https://github.com/$(format.github_repository_name)/blob/$(format.github_branch_name)/$(corrected_file_name)#L$(line_number))"
        print(io, " - **$(extended_coordinates)** $(hint)\n")
    else
        print(io, " - **$(coordinates)** $(hint)\n")
    end

    # Produce workflow command to see results in the PR file changed tab:
    # https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/workflow-commands-for-github-actions#example-setting-an-error-message
    println(format.stream_workflowcommand, "::error file=$(corrected_file_name),line=$(line_number),col=$(column_number)::$(hint)")
end

function print_report(format::MarkdownFormat, io::IO, lint_report::LintRuleReport)
    corrected_file_name = remove_prefix_from_filename(lint_report.file, format)

    coordinates = "Line $(lint_report.line), column $(lint_report.column):"
    if !isempty(format.github_branch_name) && !isempty(format.github_repository_name)
        extended_coordinates = "[$(coordinates)](https://github.com/$(format.github_repository_name)/blob/$(format.github_branch_name)/$(corrected_file_name)#L$(lint_report.line))"
        print(io, " - **$(extended_coordinates)** $(lint_report.msg) $(lint_report.file)\n")
    else
        print(io, " - **$(coordinates)** $(lint_report.msg) $(lint_report.file)\n")
    end

    # Produce workflow command to see results in the PR file changed tab:
    # https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/workflow-commands-for-github-actions#example-setting-an-error-message
    println(format.stream_workflowcommand, "::error file=$(corrected_file_name),line=$(lint_report.line),col=$(lint_report.column)::$(lint_report.msg)")
end

print_summary(::MarkdownFormat, io::IO, count_violations::Integer, count_recommendations::Integer) = nothing

does_file_server_need_to_be_initialized() = isnothing(StaticLint.global_server)
function initialize_file_server()
    StaticLint.global_server = setup_server()
    return StaticLint.global_server
end

"""
    run_lint(rootpath::String; server = global_server, io::IO=stdout, io_violations::Union{IO,Nothing}, io_recommendations::Union{IO,Nothing})

Run lint rules on a file `rootpath`, which must be an existing non-folder file. Return a
LintResult.

Example of use:
    import StaticLint
    StaticLint.run_lint("foo/bar/myfile.jl")
"""
function run_lint(
    rootpath::String;
    result::LintResult=LintResult(),
    server = global_server,
    io::Union{IO,Nothing}=stdout,
    io_violations::Union{IO,Nothing}=nothing,
    io_recommendations::Union{IO,Nothing}=nothing,
    filters::Vector{LintCodes}=essential_filters,
    formatter::AbstractFormatter=PlainFormat()
)
    # If no server is defined, then we define it.
    if does_file_server_need_to_be_initialized()
        server = initialize_file_server()
    end

    # If already linted, then we merely exit
    rootpath in result.linted_files && return result

    # If we are running Lint on a directory
    isdir(rootpath) && return _run_lint_on_dir(rootpath; result, server, io, io_violations, io_recommendations, filters, formatter)

    # Check if we have to be run on a Julia file. Simply exit if not.
    # This simplify the amount of work in GitHub Action
    endswith(rootpath, ".jl") || return result

    # We are running Lint on a Julia file
    _,hints,lint_reports = StaticLint.lint_file(rootpath, server; gethints = true)
    # isdefined(Main, :Infiltrator) && Main.infiltrate(@__MODULE__, Base.@locals, @__FILE__, @__LINE__)
    print_header(formatter, io, rootpath)


    is_violation(r::LintRuleReport) = r.rule isa ViolationExtendedRule
    is_recommendation(r::LintRuleReport) = r.rule isa RecommendationExtendedRule
    is_fatal(r::LintRuleReport) = r.rule isa FatalExtendedRule

    violation_reports = filter(is_violation, lint_reports)
    recommandations_reports = filter(is_recommendation, lint_reports)

    count_violations = length(violation_reports)
    count_recommendations = length(recommandations_reports)

    # function print_report(io::IO, report::LintRuleReport)
    #     write(io, "Line $(report.line), column $(report.column): $(report.msg) $(report.file)\n")
    # end

    io_tmp = isnothing(io_violations) ? io : io_violations
    for r in violation_reports
        if should_print_report(result)
            print_report(formatter, io_tmp, r)
            result.printout_count += 1
        end
    end

    io_tmp = isnothing(io_recommendations) ? io : io_recommendations
    for r in recommandations_reports
        if should_print_report(result)
            print_report(formatter, io_tmp, r)
            result.printout_count += 1
        end
    end



    # hint_as_strings = map(l -> l[2], hints)
    # hint_as_strings = filter(h->!should_be_filtered(h, filters), hint_as_strings)
    # function extract_msg_from_hint(m)
    #     r = match(r"(?<msg>.+)[\.\?] \H+", m)

    #     # We are now reaching a bug HERE
    #     if isnothing(r)
    #         @error "BUG FOUND IN StaticLint.jl, message from hint $(m) cannot be extracted"
    #         return "ERROR"
    #     end

    #     return r[:msg] * "."
    # end

    # @assert all(h -> typeof(h) == String, hint_as_strings)

    # # NO MORE THAN 30 VIOLATIONS AND 30 PR RECOMMENDATIONS
    # violation_hints = filter(m->rule_is_violation(extract_msg_from_hint(m)), hint_as_strings)
    # recommendation_hints = filter(m->rule_is_recommendation(extract_msg_from_hint(m)), hint_as_strings)

    # io_tmp = isnothing(io_violations) ? io : io_violations
    # filtered_and_printed_hints_violations =
    #     filter(h->filter_and_print_hint(h, result, io_tmp, filters, formatter), violation_hints)

    # io_tmp = isnothing(io_recommendations) ? io : io_recommendations

    # filtered_and_printed_hints_recommandations =
    #     filter(h->filter_and_print_hint(h, result, io_tmp, filters, formatter), recommendation_hints)

    # count_violations = length(filtered_and_printed_hints_violations)
    # count_recommendations = length(filtered_and_printed_hints_recommandations)

    # We run Lint on a single file.
    append!(result, LintResult(1, count_violations, count_recommendations, [rootpath]))
    return result
end

"""
file_name corresponds to a file name that is used to create the temporary file. This is
useful to test some rules that depends on the filename.

`directory` can be "src/Compiler". In that case, the file to be created is "tmp_julia_file.jl"
"""
function run_lint_on_text(
    source::String;
    result::LintResult=LintResult(),
    server = global_server,
    io::Union{IO,Nothing}=stdout,
    filters::Vector{LintCodes}=essential_filters,
    formatter::AbstractFormatter=PlainFormat(),
    directory::String = ""   # temporary directory to be created. If empty, let Julia decide
)
    io_violations = IOBuffer()
    io_recommendations = IOBuffer()
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
        run_lint(tmp_file_name; result, server, io, io_violations, io_recommendations, filters, formatter)
    end

    print(io, String(take!(io_violations)))
    print(io, String(take!(io_recommendations)))

    print_summary(
        formatter,
        io,
        result.violations_count,
        result.recommendations_count
    )
    print_footer(formatter, io)

    # If a directory has been provided, then it needs to be deleted, after manually deleting the file
    if !isempty(correct_directory)
        rm(tmp_file_name)
        rm(tmp_dir)
    end
end

function print_datadog_report(
    json_output::IO,
    report_as_string::String,
    files_count::Integer,
    violation_count::Integer,
    recommandation_count::Integer,
)
    event = Dict(
        :source => "StaticLint",
        :specversion => "1.0",
        :type => "result",
        :time => string(now(UTC)), #Dates.format(now(UTC), "yyyy-mm-ddTHH:MM:SSZ"), # RFC3339 format
        :data => Dict(
                    :report_as_string=>report_as_string,
                    :files_count => files_count,
                    :violation_count => violation_count,
                    :recommandation_count => recommandation_count,
                    )
    )
    println(json_output, JSON3.write(event))
end

"""
    generate_report(filenames::Vector{String}, output_filename::String;...)

Main entry point of StaticLint.jl. The function `generate_report` takes as argument a list
of files on which lint has to process. A report is generated containing the result of the
Lint analysis.

The procuded markdown report is intenteded to be posted as a comment on a GitHub PR.
Furthermore, a JSON report file is produced to feed DataDog.

Here are the arguments:

    - `filenames` is the list of all the file that have to be analyzed. From this lint
only Julia files are considered. Filenames provided to that list that do not end with `.jl`
will be simply ignored. Note that this variable is not considered if
`analyze_all_file_found_locally` is set to true.
    - `output_filename` is the file to be created that will contains the Markdown report.
If the file already exist, then the no analysis is run.
    - `json_output` is an output stream to which the JSON report has to be printed. Note
that the value provided to this variable may be overriden by `json_filename`. In the future,
the variable `json_output` can be removed.
    - `json_filename` file is a filename used to create the JSON report for DataDog
    - `github_repository` is the name of the repository, e.g., `raicode`
    - `branch_name` is a GitHub branch name, useful for the reporting
    - `file_prefix_to_remove` prefix to remove for all the file to be analyzed. This is
because GHAction creates a folder of the same name before cloning it. However, this
option can be removed in the future with a simple `cd` in that folder.
    - `analyze_all_file_found_locally`, when set to `true` the `filenames` argument  is not
used and instead all the file found locally, from `.` will be analyzed. This is used by
the github action workflow to run Lint on master.

When provided, `github_repository` and `branch_name` are used to have clickable links in
the Markdown report.
"""
function generate_report(
    filenames::Vector{String},
    output_filename::String;
    json_output::IO=stdout,
    json_filename::Union{Nothing,String}=nothing,  # Override `json_output` when not nothing
    github_repository::String="",
    branch_name::String="",
    file_prefix_to_remove::String="",
    analyze_all_file_found_locally::Bool=false,
    stream_workflowcommand::IO=stdout,
)
    if isfile(output_filename)
        @error "File $(output_filename) exist already."
        return
    end

    if !isnothing(json_filename)
        if isfile(json_filename)
            @error "File $(json_filename) exist already, cannot create json file."
            return
        end
        json_output = open(json_filename, "w")
    end

    local errors_count = 0
    local julia_filenames = filter(n->endswith(n, ".jl"), filenames)

    # Result of the whole analysis
    lint_result = LintResult()

    # If analyze_all_file_found_locally is set to true, we discard all the provided files
    # and analyze everything accessible from "."
    if analyze_all_file_found_locally
        julia_filenames = [pwd()]
    end

    open(output_filename, "w") do output_io
        println(output_io, "## Static code analyzer report")
        println(output_io, "**Output of the [StaticLint.jl code analyzer]\
            (https://github.com/RelationalAI/StaticLint.jl). \
            🫵[Want to contribute?](https://github.com/RelationalAI/StaticLint.jl/blob/main/README.md#contributing-to-staticlintjl)🫵 \
            [RelationalAI Style Guide for Julia](https://github.com/RelationalAI/RAIStyle)**\n\
            Report creation time (UTC): ($(now(UTC)))")


        formatter=MarkdownFormat(
            branch_name,
            github_repository,
            file_prefix_to_remove,
            stream_workflowcommand,
            )

        io_violations = IOBuffer()
        io_recommendations = IOBuffer()

        for filename in julia_filenames
            StaticLint.run_lint(
                filename;
                result = lint_result,
                io = output_io,
                io_violations = io_violations,
                io_recommendations = io_recommendations,
                filters = essential_filters,
                formatter
            )
        end
        print(output_io, String(take!(io_violations)))

        recommendations = String(take!(io_recommendations))
        if !isempty(recommendations)
            println(output_io, "\n")
            println(output_io, """
                                <details>
                                <summary>For PR Reviewer ($(lint_result.recommendations_count))</summary>

                                $(recommendations)
                                </details>
                                """)
        end

        has_julia_file = !isempty(lint_result.linted_files)

        if lint_result.violations_count + lint_result.recommendations_count > lint_result.printout_count
            println(output_io, "⚠️Only a subset of the violations and recommandations are here reported⚠️")
        end

        ending = length(julia_filenames) > 1 ? "s" : ""
        if !has_julia_file
            println(output_io, "No Julia file is modified or added in this PR.")
        else
            errors_count = lint_result.violations_count + lint_result.recommendations_count
            if iszero(errors_count)
                print(output_io, "🎉No potential threats are found over $(length(julia_filenames)) Julia file$(ending).👍\n\n")
            else
                s_vio = lint_result.violations_count > 1 ? "s" : ""
                s_rec = lint_result.recommendations_count > 1 ? "s" : ""
                is_or_are = errors_count == 1 ? "is" : "are"
                s_fil = lint_result.files_count > 1 ? "s" : ""
                println(output_io, "🚨**In total, $(lint_result.violations_count) rule violation$(s_vio) and $(lint_result.recommendations_count) PR reviewer recommendation$(s_rec) $(is_or_are) found over $(lint_result.files_count) Julia file$(s_fil)**🚨")
            end
        end
    end

    report_as_string = open(output_filename) do io read(io, String) end
    print_datadog_report(json_output, report_as_string, lint_result.files_count, lint_result.violations_count, lint_result.recommendations_count)

    # If a json_filename was provided, we are writing the result in json_output.
    # In that case, we need to close the stream at the end.
    if !isnothing(json_filename)
        close(json_output)
    end
end
