# growthrates plugin — uses the growthrates package if installed.
# growthrates (Petzoldt 2022): spline + parametric ODE growth model fitting.
# Citation: Petzoldt T (2022). growthrates: Estimate Growth Rates from Experimental Data. CRAN.

qres_plugin_growthrates <- function(raw_long, metadata = NULL, params = list()) {
  if (!requireNamespace('growthrates', quietly = TRUE)) {
    message('[growthrates] Package not installed — falling back to model_free.')
    res <- qres_plugin_model_free(raw_long, metadata, params)
    res$logs$message <- 'growthrates not installed; model_free used as fallback'
    return(res)
  }

  blank  <- as.numeric(params$blank %||% 0)
  method <- params$method %||% 'spline'   # 'spline' | 'easylinear' | 'baranyi'

  requireNamespace('dplyr')

  metrics <- raw_long |>
    dplyr::group_by(dataset_id, well_id) |>
    dplyr::arrange(time_h) |>
    dplyr::reframe({
      t <- time_h
      y <- pmax(value - blank, 1e-9)

      row <- tryCatch({
        fit <- switch(method,
          easylinear = growthrates::fit_easylinear(t, y),
          baranyi    = {
            p0 <- c(y0 = y[1], mumax = 1, K = max(y), h0 = 1)
            growthrates::fit_growthmodels(growthrates::grow_baranyi, p = p0, time = t, y = y)
          },
          growthrates::fit_spline(t, y)   # default: spline
        )
        cf <- tryCatch(fit@par, error = function(e) NULL)
        if (is.null(cf) || length(cf) == 0) stop('par slot empty or missing')

        mumax  <- unname(cf[grep('mumax|mu$|r$',  names(cf), ignore.case=TRUE)[1]])
        lag    <- unname(cf[grep('lag|lambda|h0$', names(cf), ignore.case=TRUE)[1]])
        asymp  <- unname(cf[grep('^K$|^A$|^ymax', names(cf), ignore.case=TRUE)[1]])

        mumax  <- if (length(mumax)  && is.finite(mumax))  mumax  else NA_real_
        lag    <- if (length(lag)    && is.finite(lag) && lag >= 0) lag else NA_real_
        asymp  <- if (length(asymp)  && is.finite(asymp))  asymp  else NA_real_
        dt     <- if (!is.na(mumax) && mumax > 0) log(2) / mumax else NA_real_

        dplyr::tibble(
          metric_name  = c('mumax_percapita', 'lag_time_h', 'asymptote_od', 'doubling_time_h'),
          metric_value = c(mumax, lag, asymp, dt),
          metric_unit  = c('1/h', 'h', 'OD', 'h'),
          model_name   = paste0('growthrates_', method)
        )
      }, error = function(e) {
        message('[growthrates] well fit error: ', conditionMessage(e))
        dplyr::tibble()
      })
      row
    })

  list(
    processed_measurements = raw_long,
    derived_metrics        = metrics,
    model_fits             = data.frame(),
    logs = list(
      message     = paste('growthrates:', method, 'fit'),
      package_ver = as.character(utils::packageVersion('growthrates')),
      citation    = 'Petzoldt T (2022). growthrates. https://cran.r-project.org/package=growthrates'
    )
  )
}
