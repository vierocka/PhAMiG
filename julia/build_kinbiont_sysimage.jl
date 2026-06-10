# Build a PackageCompiler sysimage bundling Kinbiont + CSV + DataFrames.
# This reduces Julia startup for the PhaMiG plugin from ~8-10 s to <1 s.
#
# Run from the project root:
#   /opt/julia-1.11.5/bin/julia julia/build_kinbiont_sysimage.jl
#
# Takes 5–15 minutes. Output: ~/.phamig_kinbiont.so
# Then add to ~/.Renviron:
#   KINBIONT_SYSIMAGE=/home/veve/.phamig_kinbiont.so

using PackageCompiler

sysimage_path = expanduser("~/.phamig_kinbiont.so")
precompile_file = joinpath(@__DIR__, "kinbiont_precompile.jl")

@info "Building sysimage at $sysimage_path …"
@info "This takes 5–15 minutes on first run."

create_sysimage(
    ["Kinbiont", "CSV", "DataFrames"];
    sysimage_path        = sysimage_path,
    precompile_execution_file = precompile_file
)

@info "Done. Add to ~/.Renviron:"
@info "  KINBIONT_SYSIMAGE=$sysimage_path"
