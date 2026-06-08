# Model-free growth estimators. No external packages required.
# Metrics: mumax (per-capita), lag, doubling_time, auc, max_od, delta_od, time_to_max.

.mf_per_capita <- function(t, y, blank = 0) {
  y <- pmax(y - blank, 1e-6)
  ly <- log(y)
  n  <- length(t)
  if (n < 3) return(rep(NA_real_, n))
  mu <- numeric(n)
  mu[1]   <- (ly[2]  - ly[1])  / (t[2]  - t[1])
  mu[n]   <- (ly[n]  - ly[n-1]) / (t[n]  - t[n-1])
  for (i in 2:(n-1)) mu[i] <- (ly[i+1] - ly[i-1]) / (t[i+1] - t[i-1])
  mu
}

.mf_lag <- function(t, mu, mumax, y0, blank = 0) {
  # Intersection of tangent at max growth rate with y = y0 (baseline)
  i_max <- which.max(mu)
  if (length(i_max) == 0 || is.na(mumax) || mumax <= 0) return(NA_real_)
  # Tangent line: log(y) = mumax * (t - t_max) + log(y_at_tmax)
  # crosses log(y0) at: t_lag = t_max - (log(y_at_tmax) - log(y0)) / mumax
  tryCatch({
    y_tmax <- pmax(y0, 1e-6)
    t[i_max] - (log(y_tmax) - log(pmax(y0, 1e-6))) / mumax
  }, error = function(e) NA_real_)
}

qres_plugin_model_free <- function(raw_long, metadata = NULL, params = list()) {
  requireNamespace('dplyr')
  blank <- as.numeric(params$blank %||% 0)

  metrics <- raw_long |>
    dplyr::group_by(dataset_id, well_id) |>
    dplyr::arrange(time_h) |>
    dplyr::reframe({
      t  <- time_h
      y  <- value
      n  <- length(t)

      mu      <- .mf_per_capita(t, y, blank)
      mumax   <- if (n >= 3) max(mu, na.rm = TRUE) else NA_real_
      y0      <- y[1]
      lag     <- if (n >= 3) .mf_lag(t, mu, mumax, y0, blank) else NA_real_
      lag     <- ifelse(is.finite(lag) & lag >= 0, lag, NA_real_)
      dt      <- if (is.finite(mumax) && mumax > 0) log(2) / mumax else NA_real_
      auc     <- if (n >= 2) sum(diff(t) * (head(y,-1) + tail(y,-1)) / 2, na.rm=TRUE) else NA_real_
      max_od  <- max(y, na.rm = TRUE)
      delta   <- max_od - y0
      t_max   <- t[which.max(y)]

      dplyr::tibble(
        metric_name  = c('mumax_percapita', 'lag_time_h', 'doubling_time_h',
                          'auc_trapezoid', 'max_od', 'delta_od', 'time_to_max_h'),
        metric_value = c(mumax, lag, dt, auc, max_od, delta, t_max),
        metric_unit  = c('1/h', 'h', 'h', 'OD*h', 'OD', 'OD', 'h'),
        model_name   = 'model_free'
      )
    })

  list(
    processed_measurements = raw_long,
    derived_metrics        = metrics,
    model_fits             = data.frame(),
    logs                   = list(message = 'model_free: finite-difference per-capita rates')
  )
}
