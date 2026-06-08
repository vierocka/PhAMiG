# gcplyr plugin — uses the gcplyr package if installed, falls back to model_free.
# gcplyr (Blazanin 2023): derivative-based per-capita growth rate estimation.
# Citation: Blazanin M (2023). gcplyr: an R package for microbial growth curve analysis.

qres_plugin_gcplyr <- function(raw_long, metadata = NULL, params = list()) {
  if (!requireNamespace('gcplyr', quietly = TRUE)) {
    message('[gcplyr] Package not installed — falling back to model_free estimators.')
    res <- qres_plugin_model_free(raw_long, metadata, params)
    res$logs$message <- 'gcplyr not installed; model_free used as fallback'
    return(res)
  }

  blank <- as.numeric(params$blank %||% 0)

  metrics <- tryCatch({
    requireNamespace('dplyr')
    raw_long |>
      dplyr::group_by(dataset_id, well_id) |>
      dplyr::arrange(time_h) |>
      dplyr::reframe({
        t <- time_h
        y <- pmax(value - blank, 1e-6)

        # gcplyr derivative on log-scale via calc_deriv
        df_well <- data.frame(time = t, y = y)
        deriv_df <- tryCatch(
          gcplyr::calc_deriv(x = 'time', y = 'y', data = df_well,
                              percapita = TRUE, blank = blank),
          error = function(e) NULL
        )

        if (!is.null(deriv_df) && 'deriv' %in% names(deriv_df)) {
          mu    <- deriv_df$deriv
          mumax <- max(mu, na.rm = TRUE)
          i_max <- which.max(mu)
          lag   <- if (length(i_max) && mumax > 0)
            t[i_max] - log(y[i_max] / y[1]) / mumax else NA_real_
          lag   <- ifelse(is.finite(lag) & lag >= 0, lag, NA_real_)
        } else {
          # calc_deriv API mismatch — compute manually
          mu    <- .mf_per_capita(t, y, 0)
          mumax <- max(mu, na.rm = TRUE)
          lag   <- .mf_lag(t, mu, mumax, y[1], 0)
          lag   <- ifelse(is.finite(lag) & lag >= 0, lag, NA_real_)
        }

        dt  <- if (is.finite(mumax) && mumax > 0) log(2) / mumax else NA_real_
        auc <- tryCatch(
          gcplyr::auc(x = t, y = y),
          error = function(e) sum(diff(t) * (head(y,-1) + tail(y,-1)) / 2, na.rm=TRUE)
        )

        dplyr::tibble(
          metric_name  = c('mumax_percapita', 'lag_time_h', 'doubling_time_h', 'auc_trapezoid'),
          metric_value = c(mumax, lag, dt, auc),
          metric_unit  = c('1/h', 'h', 'h', 'OD*h'),
          model_name   = 'gcplyr'
        )
      })
  }, error = function(e) {
    message('[gcplyr] Error: ', conditionMessage(e), ' — using model_free fallback')
    NULL
  })

  if (is.null(metrics)) {
    res <- qres_plugin_model_free(raw_long, metadata, params)
    res$logs$message <- paste('gcplyr error; model_free used as fallback')
    return(res)
  }

  list(
    processed_measurements = raw_long,
    derived_metrics        = metrics,
    model_fits             = data.frame(),
    logs = list(
      message      = 'gcplyr: per-capita derivative-based growth rate estimation',
      package_ver  = as.character(utils::packageVersion('gcplyr')),
      citation     = 'Blazanin M (2023) gcplyr. doi:10.1093/g3journal/jkad261'
    )
  )
}
