# KinBiont plugin — bridges to Kinbiont.jl via the JuliaCall R package.
# KinBiont.jl (Pinheiro group): log-linear growth modelling in Julia.
#
# Real API (v1.1.8):
#   fitting_one_well_Log_Lin(data_matrix, well_name, plate_name)
#   data_matrix: Matrix{Float64} row-oriented → row 1 = time, row 2 = OD
#   Returns 5-tuple; element [2] is a Vector with:
#     [1]=label_exp  [2]=name_well  [3]=start_exp_win  [4]=end_exp_win
#     [5]=max_specific_GR  [6]=specific_GR
#
# Citation: Pinheiro group. Kinbiont.jl.
#   https://github.com/pinheiroGroup/Kinbiont.jl
#
# Setup:
#   install.packages("JuliaCall")
#   library(JuliaCall); julia_setup()
#   In Julia: using Pkg; Pkg.add("Kinbiont")

# Memoize: julia_setup() called at most once per R session
.KINBIONT_CACHE <- new.env(parent = emptyenv())
.KINBIONT_CACHE$ready <- NA   # NA = unchecked, TRUE = ok, FALSE = failed

.kinbiont_julia_home <- function() {
  # Prefer explicit env var, then probe known install locations
  env_home <- Sys.getenv('JULIA_HOME', unset = '')
  if (nzchar(env_home) && file.exists(file.path(env_home, 'julia')))
    return(env_home)
  candidates <- c(
    path.expand('~/Desktop/SW/julia-1.9.4/usr/bin'),
    path.expand('~/Desktop/SW/julia-1.9.4/bin'),
    path.expand('~/Desktop/SW/julia-1.11.5/bin'),
    path.expand('~/Desktop/SW/julia-1.11.5/usr/bin'),
    '/usr/local/bin', '/usr/bin'
  )
  for (p in candidates)
    if (file.exists(file.path(p, 'julia'))) return(p)
  ''
}

.kinbiont_julia_ready <- function() {
  # Return cached result — avoids repeated julia_setup() calls that segfault
  if (!is.na(.KINBIONT_CACHE$ready)) return(isTRUE(.KINBIONT_CACHE$ready))
  if (!requireNamespace('JuliaCall', quietly = TRUE)) {
    .KINBIONT_CACHE$ready <- FALSE; return(FALSE)
  }
  result <- tryCatch({
    jh <- .kinbiont_julia_home()
    if (nzchar(jh))
      JuliaCall::julia_setup(JULIA_HOME = jh, installJulia = FALSE)
    else
      JuliaCall::julia_setup(installJulia = FALSE)
    JuliaCall::julia_eval('using Kinbiont; true')
  }, error = function(e) {
    message('[kinbiont] Julia setup failed: ', conditionMessage(e),
            '\n  Fix: download generic Julia 1.11 binaries from julialang.org',
            ' (system CHOLMOD 5.2 incompatible with Julia 1.9 CHOLMOD 3.0)')
    FALSE
  })
  .KINBIONT_CACHE$ready <- isTRUE(result)
  isTRUE(result)
}

qres_plugin_kinbiont <- function(raw_long, metadata = NULL, params = list()) {
  if (!.kinbiont_julia_ready()) {
    return(list(
      processed_measurements = raw_long,
      derived_metrics        = data.frame(),
      model_fits             = data.frame(),
      logs = list(
        message  = paste(
          'KinBiont.jl not available.',
          'Install Julia, then in R:',
          'install.packages("JuliaCall"); JuliaCall::julia_setup();',
          'JuliaCall::julia_eval(\'using Pkg; Pkg.add("Kinbiont")\')'
        ),
        status   = 'not_available',
        citation = 'Pinheiro group. Kinbiont.jl. https://github.com/pinheiroGroup/Kinbiont.jl'
      )
    ))
  }

  requireNamespace('dplyr')
  blank <- as.numeric(params$blank %||% 0)

  metrics <- raw_long |>
    dplyr::group_by(dataset_id, well_id) |>
    dplyr::arrange(time_h) |>
    dplyr::reframe({
      t <- time_h
      y <- pmax(value - blank, 1e-9)

      tryCatch({
        # Build row-oriented matrix: row 1 = time, row 2 = OD (Kinbiont convention)
        JuliaCall::julia_assign('kb_t', t)
        JuliaCall::julia_assign('kb_y', y)
        JuliaCall::julia_eval('kb_mat = Matrix{Float64}(hcat(kb_t, kb_y)\')')

        well_jl  <- as.character(well_id[1])
        plate_jl <- as.character(dataset_id[1])
        JuliaCall::julia_assign('kb_well',  well_jl)
        JuliaCall::julia_assign('kb_plate', plate_jl)

        JuliaCall::julia_eval(
          'kb_res = fitting_one_well_Log_Lin(kb_mat, kb_well, kb_plate)'
        )

        # result[2] = parameter vector (Julia 1-indexed):
        #   [3]=start_exp_win, [4]=end_exp_win, [5]=max_specific_GR, [6]=specific_GR
        mumax       <- JuliaCall::julia_eval('Float64(kb_res[2][5])')
        lag_h       <- JuliaCall::julia_eval('Float64(kb_res[2][3])')
        end_exp_h   <- JuliaCall::julia_eval('Float64(kb_res[2][4])')
        specific_gr <- JuliaCall::julia_eval('Float64(kb_res[2][6])')

        dt <- if (is.finite(mumax) && mumax > 0) log(2) / mumax else NA_real_

        dplyr::tibble(
          metric_name  = c('mumax_percapita', 'lag_time_h', 'doubling_time_h',
                           'exp_window_end_h', 'specific_gr'),
          metric_value = c(mumax, lag_h, dt, end_exp_h, specific_gr),
          metric_unit  = c('1/h', 'h', 'h', 'h', '1/h'),
          model_name   = 'kinbiont_loglin'
        )
      }, error = function(e) {
        message('[kinbiont] Julia fit error for well ', well_id[1],
                ': ', conditionMessage(e))
        dplyr::tibble()
      })
    })

  list(
    processed_measurements = raw_long,
    derived_metrics        = metrics,
    model_fits             = data.frame(),
    logs = list(
      message  = if (nrow(metrics) > 0)
        paste('KinBiont.jl log-lin: fitted',
              length(unique(metrics$well_id)), 'curves')
      else
        'KinBiont.jl fit returned no results; check messages above',
      citation = 'Pinheiro group. Kinbiont.jl. https://github.com/pinheiroGroup/Kinbiont.jl',
      github   = 'https://github.com/pinheiroGroup/Kinbiont.jl'
    )
  )
}
