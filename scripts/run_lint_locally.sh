#!/bin/bash

# If file exist, typically from a previous run, then we remove it
if [ -f /tmp/files_to_run_lint.txt ]; then
   rm /tmp/files_to_run_lint.txt
fi

# If the file containing result of lint exist, then we remove it
if [ -f /tmp/result_lint.txt ]; then
   rm /tmp/result_lint.txt
fi

# If no argument is provided, then we simply use the files staged
if [[ $# -eq 0 ]] ; then
    echo 'No argument provided, running on staged files'
    FILES_LOCALLY_ADDED=`git status --porcelain | awk 'match($1, "A"){print $2}'`
    FILES_LOCALLY_MODIFIED=`git status --porcelain | awk 'match($1, "M"){print $2}'`
    echo ${FILES_LOCALLY_ADDED} > /tmp/files_to_run_lint.txt
    echo ${FILES_LOCALLY_MODIFIED} >> /tmp/files_to_run_lint.txt
else
    # If some files are provided, then we use these
    echo $@ >> /tmp/files_to_run_lint.txt
    # echo "RUNNING LINT ON: "
    # cat /tmp/files_to_run_lint.txt
    # echo "---"
fi

# Running StaticLint
echo "CURRENT PATH=" $PWD

echo "About to run StaticLint.jl..."
julia --project=. -e "
  using StaticLint
  result = StaticLint.LintResult()
  all_files_tmp=split(open(io->read(io, String), \"/tmp/files_to_run_lint.txt\", \"r\"))
  # convert substring into string
  all_files=map(string, all_files_tmp)
  # filter to existing Julia files only
  all_files=filter(isfile, all_files)
  all_files=filter(f->endswith(f, \".jl\"), all_files)

  @info \"Running lint on \$(length(all_files)) files\"

  formatter = StaticLint.PreCommitFormat()
  # Run lint on all files
  for f in all_files
    StaticLint.run_lint(f; result, formatter)
  end

  # Return an error if there is an unsafe log
  if result.fatalviolations_count > 0
    StaticLint.print_summary(formatter, stdout, result)
    @error \"Fatal error discovered\"
    exit(1)
  end
  exit(0)
"
