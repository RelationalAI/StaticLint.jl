#!/bin/bash

# EXPECT StaticLint.jl TO BE PRESENT.
cd StaticLint.jl
julia --proj -e "import Pkg ; Pkg.Registry.update() ; Pkg.instantiate() ; Pkg.build()"
cd ..

# WRITING FILES ON WHICH LINT SHOULD BE RUN
echo "FILES TO BE LINTED"
cat files_to_run_lint.txt

# RUNNING THE CHECK
julia --project=StaticLint.jl -e "
  using StaticLint
  StaticLint.generate_report(readlines(\"files_to_run_lint.txt\"), \"result.txt\")
"

# SHOW THE RESULTS ON GITHUB ACTION. USEFUL FOR DEBUGGING
echo "HERE ARE THE RESULTS:"
cat result.txt
echo "END OF RESULTS"