# omniplate plugin — bridges to Python omniplate via reticulate.
# omniplate: GP-derivative-based growth parameter estimation.
# Citation: omniplate. https://github.com/naef-lab/omniplate
#
# Setup (done):
#   conda create -n omniplate_env python=3.11
#   conda run -n omniplate_env pip install omniplate
#   R: install.packages("reticulate")
#
# Key output metric: local_max_dOD/dt = maximum absolute growth rate (OD/h)
# Per-capita equivalent: divide by OD at that time point

OMNIPLATE_CONDA_ENV <- 'omniplate_env'
OMNIPLATE_CONDA_BIN <- path.expand('~/miniconda3/bin/conda')

.OMNIPLATE_CACHE <- new.env(parent = emptyenv())
.OMNIPLATE_CACHE$ready <- NA

.omniplate_py_ready <- function() {
  # Return TRUE immediately if already confirmed ready in this session.
  if (isTRUE(.OMNIPLATE_CACHE$ready)) return(TRUE)
  if (!requireNamespace('reticulate', quietly = TRUE)) return(FALSE)

  conda_python <- file.path(
    path.expand('~/miniconda3'), 'envs', OMNIPLATE_CONDA_ENV, 'bin', 'python')

  result <- tryCatch({
    # If Python not yet started, point it at the conda env.
    if (!reticulate::py_available(initialize = FALSE)) {
      if (file.exists(conda_python)) {
        reticulate::use_python(conda_python, required = FALSE)
      } else {
        reticulate::use_condaenv(OMNIPLATE_CONDA_ENV,
                                  conda  = OMNIPLATE_CONDA_BIN,
                                  required = FALSE)
      }
    }
    reticulate::py_module_available('omniplate')
  }, error = function(e) {
    message('[omniplate] Python check failed: ', conditionMessage(e))
    FALSE
  })

  # Only cache TRUE; let FALSE re-check on next call (env may not be ready yet).
  if (isTRUE(result)) .OMNIPLATE_CACHE$ready <- TRUE
  isTRUE(result)
}

qres_plugin_omniplate <- function(raw_long, metadata = NULL, params = list()) {
  if (!.omniplate_py_ready()) {
    return(list(
      processed_measurements = raw_long,
      derived_metrics        = data.frame(),
      model_fits             = data.frame(),
      logs = list(
        message = paste(
          'omniplate not available.',
          'Requires conda env "omniplate_env" with Python 3.11 and omniplate installed.',
          'See R/plugins/omniplate.R for setup instructions.'),
        status  = 'not_available',
        citation = 'naef-lab/omniplate. https://github.com/naef-lab/omniplate'
      )
    ))
  }

  requireNamespace('dplyr')
  requireNamespace('reticulate')

  blank <- as.numeric(params$blank %||% 0)

  # Import Python modules once
  run_fit <- reticulate::import('omniplate.omfitderiv')$run_fit_deriv
  np      <- reticulate::import('numpy')

  metrics <- raw_long |>
    dplyr::group_by(dataset_id, well_id) |>
    dplyr::arrange(time_h) |>
    dplyr::reframe({
      t <- time_h
      y <- pmax(value - blank, 1e-9)

      tryCatch({
        t_np <- np$array(t)
        y_np <- np$array(y)

        res <- run_fit(
          t_np, y_np,
          fitvar    = 'OD',
          derivname = 'dOD/dt',
          experiment = dataset_id[1],
          condition  = 'growth',
          strain     = well_id[1],
          no_runs   = as.integer(params$no_runs   %||% 3L),
          no_inits  = as.integer(params$no_inits  %||% 50L),
          exit_early = TRUE,
          logs       = TRUE
        )

        sc   <- res$dict_for_sc
        mumax_abs <- as.numeric(sc[['local_max_dOD/dt']])        # OD/h
        t_mumax   <- as.numeric(sc[['time_of_local_max_dOD/dt']])# h
        max_od    <- as.numeric(sc[['max_OD']])
        auc       <- as.numeric(sc[['area_under_OD']])

        # Per-capita: mu = (dOD/dt) / OD  ≈ mumax_abs / OD_at_t_mumax
        od_at_peak <- y[which.min(abs(t - t_mumax))]
        mumax_pc   <- if (is.finite(od_at_peak) && od_at_peak > 0)
          mumax_abs / od_at_peak else NA_real_
        dt <- if (is.finite(mumax_pc) && mumax_pc > 0) log(2) / mumax_pc else NA_real_

        dplyr::tibble(
          metric_name  = c('mumax_abs_od_per_h', 'mumax_percapita', 'doubling_time_h',
                           'max_od', 'auc_trapezoid', 'time_to_max_growth_h'),
          metric_value = c(mumax_abs, mumax_pc, dt, max_od, auc, t_mumax),
          metric_unit  = c('OD/h', '1/h', 'h', 'OD', 'OD*h', 'h'),
          model_name   = 'omniplate_gp'
        )
      }, error = function(e) {
        message('[omniplate] Fit error for well ', well_id[1], ': ', conditionMessage(e))
        dplyr::tibble()
      })
    })

  list(
    processed_measurements = raw_long,
    derived_metrics        = metrics,
    model_fits             = data.frame(),
    logs = list(
      message  = if (nrow(metrics) > 0)
        paste('omniplate GP-deriv: fitted', length(unique(metrics$well_id)), 'curves')
      else
        'omniplate returned no results',
      citation = 'naef-lab/omniplate. https://github.com/naef-lab/omniplate'
    )
  )
}
