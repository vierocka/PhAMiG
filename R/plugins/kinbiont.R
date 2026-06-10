# KinBiont plugin — calls Kinbiont.jl via Julia subprocess (system2).
#
# Why subprocess instead of JuliaCall:
#   JuliaCall 0.17.x segfaults with Julia 1.11+ because Julia changed its
#   C embedding API. system2() completely sidesteps embedding: it starts Julia
#   as a child process, runs the script, reads stdout.
#
# Startup overhead:
#   Cold Julia start: ~5–10 s. With a PackageCompiler sysimage it drops to
#   <0.5 s. Build one with:
#     julia -e 'using PackageCompiler; create_sysimage(["Kinbiont"];
#               sysimage_path=expanduser("~/.phamig_kinbiont.so"),
#               precompile_execution_file="path/to/kinbiont_precompile.jl")'
#   Then set env var KINBIONT_SYSIMAGE=~/.phamig_kinbiont.so.
#
# Kinbiont.jl (Pinheiro group):
#   Paper:  https://www.nature.com/articles/s41467-025-61592-6
#   GitHub: https://github.com/pinheiroGroup/Kinbiont.jl
#   bio.tools: https://bio.tools/kinbiont
#
# Required Julia setup (≥1.11):
#   julia -e 'using Pkg; Pkg.add(url="https://github.com/pinheiroGroup/Kinbiont.jl")'

.KINBIONT_CACHE <- new.env(parent = emptyenv())
.KINBIONT_CACHE$ready <- NA   # NA = unchecked, TRUE = ok, FALSE = failed

.kinbiont_julia_bin <- function() {
  env_home <- Sys.getenv('JULIA_HOME', unset = '')
  if (nzchar(env_home)) {
    p <- file.path(env_home, 'julia')
    if (file.exists(p)) return(p)
  }
  candidates <- c(
    '/opt/julia-1.11.5/bin/julia',
    '/opt/julia-1.11.1/bin/julia',
    '/opt/julia-1.11.0/bin/julia',
    '/usr/local/bin/julia',
    '/usr/bin/julia'
  )
  for (p in candidates) if (file.exists(p)) return(p)
  bin <- Sys.which('julia')
  if (nzchar(bin)) bin else ''
}

.kinbiont_julia_version <- function(julia_bin) {
  out <- tryCatch(
    system2(julia_bin, '--version', stdout = TRUE, stderr = FALSE),
    error = function(e) ''
  )
  m <- regmatches(out, regexpr('\\d+\\.\\d+\\.\\d+', out))
  if (length(m) == 1) package_version(m) else NA
}

.kinbiont_julia_ready <- function() {
  if (!is.na(.KINBIONT_CACHE$ready)) return(isTRUE(.KINBIONT_CACHE$ready))

  julia_bin <- .kinbiont_julia_bin()
  if (!nzchar(julia_bin)) {
    .KINBIONT_CACHE$ready <- FALSE
    message('[kinbiont] Julia not found. Install Julia 1.11+ from https://julialang.org/downloads/')
    return(FALSE)
  }

  ver <- .kinbiont_julia_version(julia_bin)
  if (!is.na(ver) && ver < package_version('1.11.0')) {
    .KINBIONT_CACHE$ready <- FALSE
    message(sprintf(
      '[kinbiont] Julia %s found but 1.11+ required (JuliaCall segfaults on 1.9/1.10).\n  Download: https://julialang.org/downloads/',
      as.character(ver)))
    return(FALSE)
  }

  # Quick smoke-test: can Julia load Kinbiont?
  tmp_test <- tempfile(fileext = '.jl')
  writeLines('using Kinbiont; print("ok")', tmp_test)
  on.exit(unlink(tmp_test), add = TRUE)
  test_out <- tryCatch(
    system2(julia_bin, c('--quiet', tmp_test),
            stdout = TRUE, stderr = FALSE, timeout = 60),
    error = function(e) ''
  )
  ok <- identical(trimws(paste(test_out, collapse = '')), 'ok')
  if (!ok) {
    .KINBIONT_CACHE$ready <- FALSE
    message('[kinbiont] Kinbiont.jl not available. In Julia: using Pkg; Pkg.add(url="https://github.com/pinheiroGroup/Kinbiont.jl")')
    return(FALSE)
  }
  .KINBIONT_CACHE$ready <- TRUE
  TRUE
}

# Julia batch script: reads wells.csv (well_id, time_h, value),
# fits each well, prints one TSV row per well.
.kinbiont_julia_script <- '
import CSV, DataFrames
using Kinbiont

df = CSV.read(ARGS[1], DataFrames.DataFrame)

println("well_id\tmumax\tlag_h\tend_exp_h\tspecific_gr\tok")
for grp in DataFrames.groupby(df, :well_id)
    wid = String(string(grp.well_id[1]))
    t   = Float64.(grp.time_h)
    y   = Float64.(grp.value)
    y   = max.(y, 1e-9)
    try
        mat = Matrix{Float64}(hcat(t, y)\')
        res = fitting_one_well_Log_Lin(mat, wid, "phamig")
        p   = res[2]
        println(wid, "\t", p[5], "\t", p[3], "\t", p[4], "\t", p[6], "\ttrue")
    catch e
        println(wid, "\tNA\tNA\tNA\tNA\tfalse\t", replace(string(e), "\t"=>" "))
    end
end
'

KINBIONT_MIN_TIMEPOINTS <- 7L

qres_plugin_kinbiont <- function(raw_long, metadata = NULL, params = list()) {
  n_tp <- length(unique(raw_long$time_h))
  if (n_tp < KINBIONT_MIN_TIMEPOINTS) {
    msg <- sprintf(
      'KinBiont requires ≥%d time points per well; only %d found — skipped.',
      KINBIONT_MIN_TIMEPOINTS, n_tp)
    message('[kinbiont] ', msg)
    return(list(
      processed_measurements = raw_long,
      derived_metrics        = data.frame(),
      model_fits             = data.frame(),
      logs = list(message = msg, status = 'skipped_insufficient_timepoints')
    ))
  }

  if (!.kinbiont_julia_ready()) {
    return(list(
      processed_measurements = raw_long,
      derived_metrics        = data.frame(),
      model_fits             = data.frame(),
      logs = list(
        message  = paste(
          'KinBiont.jl not available.',
          'Install Julia 1.11+ from https://julialang.org/downloads/,',
          'then in Julia: using Pkg; Pkg.add(url="https://github.com/pinheiroGroup/Kinbiont.jl")'
        ),
        status   = 'not_available',
        citation = 'Pinheiro et al. (2025) Nature Communications. https://www.nature.com/articles/s41467-025-61592-6',
        github   = 'https://github.com/pinheiroGroup/Kinbiont.jl'
      )
    ))
  }

  requireNamespace('dplyr')
  blank <- as.numeric(params$blank %||% 0)

  # Prepare per-well data table
  wells_dt <- raw_long |>
    dplyr::arrange(well_id, time_h) |>
    dplyr::mutate(value = pmax(value - blank, 1e-9)) |>
    dplyr::select(well_id, time_h, value)

  tmp_csv    <- tempfile(fileext = '.csv')
  tmp_script <- tempfile(fileext = '.jl')
  on.exit({ unlink(tmp_csv); unlink(tmp_script) }, add = TRUE)

  utils::write.csv(wells_dt, tmp_csv, row.names = FALSE)
  writeLines(.kinbiont_julia_script, tmp_script)

  julia_bin <- .kinbiont_julia_bin()
  sysimage  <- Sys.getenv('KINBIONT_SYSIMAGE', unset = '')
  julia_args <- c(
    if (nzchar(sysimage) && file.exists(sysimage)) c('--sysimage', sysimage),
    '--quiet', tmp_script, tmp_csv
  )

  message('[kinbiont] Running Julia (first call may take ~10 s for precompilation)…')
  raw_out <- tryCatch(
    system2(julia_bin, julia_args, stdout = TRUE, stderr = FALSE,
            timeout = 300),
    error = function(e) {
      message('[kinbiont] system2 error: ', conditionMessage(e))
      character(0)
    }
  )

  if (length(raw_out) == 0) {
    return(list(
      processed_measurements = raw_long,
      derived_metrics        = data.frame(),
      model_fits             = data.frame(),
      logs = list(message = 'KinBiont.jl returned no output; check Julia installation.',
                  status = 'error')
    ))
  }

  # Parse TSV output — skip header line
  parsed <- tryCatch({
    con <- textConnection(paste(raw_out, collapse = '\n'))
    on.exit(close(con), add = TRUE)
    df <- utils::read.table(con, header = TRUE, sep = '\t',
                            stringsAsFactors = FALSE, fill = TRUE,
                            quote = '')
    df
  }, error = function(e) NULL)

  if (is.null(parsed) || nrow(parsed) == 0) {
    return(list(
      processed_measurements = raw_long,
      derived_metrics        = data.frame(),
      model_fits             = data.frame(),
      logs = list(message = 'Could not parse KinBiont output.',
                  status = 'error')
    ))
  }

  # Build tidy metrics
  metrics <- do.call(rbind, lapply(seq_len(nrow(parsed)), function(i) {
    row <- parsed[i, ]
    if (!isTRUE(as.logical(row$ok))) return(NULL)
    mumax       <- suppressWarnings(as.numeric(row$mumax))
    lag_h       <- suppressWarnings(as.numeric(row$lag_h))
    end_exp_h   <- suppressWarnings(as.numeric(row$end_exp_h))
    specific_gr <- suppressWarnings(as.numeric(row$specific_gr))
    dt <- if (is.finite(mumax) && mumax > 0) log(2) / mumax else NA_real_
    data.frame(
      well_id      = row$well_id,
      dataset_id   = if ('dataset_id' %in% names(raw_long))
                       raw_long$dataset_id[raw_long$well_id == row$well_id][1] else NA,
      metric_name  = c('mumax_percapita', 'lag_time_h', 'doubling_time_h',
                       'exp_window_end_h', 'specific_gr'),
      metric_value = c(mumax, lag_h, dt, end_exp_h, specific_gr),
      metric_unit  = c('1/h', 'h', 'h', 'h', '1/h'),
      model_name   = 'kinbiont_loglin',
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(metrics)) metrics <- data.frame()

  list(
    processed_measurements = raw_long,
    derived_metrics        = metrics,
    model_fits             = data.frame(),
    logs = list(
      message  = if (nrow(metrics) > 0)
        paste('KinBiont.jl log-lin: fitted',
              length(unique(metrics$well_id)), 'wells')
      else
        'KinBiont.jl returned no successful fits; check messages above',
      citation = 'Pinheiro et al. (2025) Nature Communications. https://www.nature.com/articles/s41467-025-61592-6',
      github   = 'https://github.com/pinheiroGroup/Kinbiont.jl',
      biotools = 'https://bio.tools/kinbiont'
    )
  )
}
