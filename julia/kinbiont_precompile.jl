# Precompile execution script for PackageCompiler.
# Exercises the exact code path used by the PhaMiG Kinbiont plugin so that
# all compilation work is captured in the sysimage.
#
# Build the sysimage (run once, takes ~5-10 min):
#   julia julia/build_kinbiont_sysimage.jl
# After building, set in ~/.Renviron:
#   KINBIONT_SYSIMAGE=/home/veve/.phamig_kinbiont.so

import CSV, DataFrames
using Kinbiont

# Representative growth curve — uniform 0.25 h spacing, 17 points.
# Verified to produce a successful fit; sparse data can cause BoundsError.
t = collect(0.0:0.25:4.0)
y = [0.05, 0.055, 0.062, 0.075, 0.098, 0.165, 0.31, 0.58, 1.0, 1.6, 2.0, 2.4, 2.6, 2.7, 2.75, 2.78, 2.8]

mat = Matrix{Float64}(hcat(t, y)')
res = fitting_one_well_Log_Lin(mat, "A1", "precompile_run")
_ = res[2][5]   # mumax — touch the result to ensure full dispatch is compiled

# Also exercise the CSV read path used by the plugin
tmp = tempname() * ".csv"
df  = DataFrames.DataFrame(well_id = ["A1"; "A1"], time_h = [0.0; 1.0], value = [0.05; 0.1])
CSV.write(tmp, df)
df2 = CSV.read(tmp, DataFrames.DataFrame)
rm(tmp)
