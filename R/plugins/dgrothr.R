# DGrowthR plugin — Gaussian Process Regression growth curve analysis.
#
# DGrowthR (Feldl & Olayo-Alarcon): GPR-based growth modelling, polyauxic
# detection, and differential testing.
#
# Author: Medina Feldl <mail@medinabajramovic.de>
# Citation: Feldl M & Olayo-Alarcon R. DGrowthR: Comprehensive Toolset for
#   Bacterial Growth Curve Analysis via Gaussian Processes.
#   https://bio-datascience.github.io/DGrowthR/
# Blog:   https://medinabajramovic.de/differential-growth-curve-analysis/
# GitHub: https://github.com/bio-datascience/DGrowthR/
#
# Install:
#   remotes::install_github("bio-datascience/DGrowthR")

.dgrothr_ready <- function() requireNamespace('DGrowthR', quietly = TRUE)

# Convert Qres raw_long to DGrowthR S4 object (one well = one curve_id)
.qres_to_dgrothr <- function(raw_long, blank = 0) {
  requireNamespace('DGrowthR')
  requireNamespace('dplyr')

  od_df <- raw_long |>
    dplyr::arrange(well_id, time_h) |>
    dplyr::group_by(well_id) |>
    dplyr::mutate(timepoint_n = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      timepoint   = time_h,
      timepoint_n = timepoint_n,
      od          = pmax(value - blank, 1e-9),
      curve_id    = well_id
    )

  meta_df <- data.frame(
    curve_id   = unique(od_df$curve_id),
    dataset_id = unique(raw_long$dataset_id)[1],
    stringsAsFactors = FALSE
  )

  DGrowthR::DGrowthR(od_data = od_df, metadata = meta_df, raw_od = od_df)
}

qres_plugin_dgrothr <- function(raw_long, metadata = NULL, params = list()) {
  if (!.dgrothr_ready()) {
    return(list(
      processed_measurements = raw_long,
      derived_metrics        = data.frame(),
      model_fits             = data.frame(),
      logs = list(
        message  = paste(
          'DGrowthR not installed.',
          'Install via: remotes::install_github("bio-datascience/DGrowthR")'
        ),
        status   = 'not_available',
        citation = 'Feldl M & Olayo-Alarcon R. https://bio-datascience.github.io/DGrowthR/',
        blog     = 'https://medinabajramovic.de/differential-growth-curve-analysis/'
      )
    ))
  }

  requireNamespace('dplyr')
  blank  <- as.numeric(params$blank %||% 0)
  n_cores <- as.integer(params$n_cores %||% 1L)

  metrics <- tryCatch({
    obj <- .qres_to_dgrothr(raw_long, blank)

    obj <- DGrowthR::preprocess_data(obj,
      baseline     = 1,
      log_transform = TRUE,
      skip_first_n_timepoints = 0
    )

    obj <- DGrowthR::estimate_growth_parameters(obj,
      model_covariate = 'curve_id',
      predict_n_steps = 80,
      n_cores         = n_cores,
      save_gp_data    = FALSE
    )

    gp <- obj@growth_parameters
    if (is.null(gp) || nrow(gp) == 0) stop('No growth parameters returned')

    # DGrowthR column names → Qres metric names
    col_map <- list(
      max_growth_rate  = list(name = 'mumax_percapita',  unit = '1/h'),
      end_lagphase     = list(name = 'lag_time_h',       unit = 'h'),
      max_od           = list(name = 'max_od',           unit = 'OD'),
      auc              = list(name = 'auc_trapezoid',    unit = 'OD*h'),
      max_death_rate   = list(name = 'max_death_rate',   unit = '1/h'),
      start_statphase  = list(name = 'start_statphase_h', unit = 'h')
    )

    # Pivot to long format — one row per well × metric
    rows <- lapply(names(col_map), function(col) {
      if (!col %in% names(gp)) return(NULL)
      m <- col_map[[col]]
      dplyr::tibble(
        well_id      = gp$gpfit_id,
        metric_name  = m$name,
        metric_value = as.numeric(gp[[col]]),
        metric_unit  = m$unit,
        model_name   = 'dgrothr_gp'
      )
    })
    combined <- dplyr::bind_rows(Filter(Negate(is.null), rows))

    # Derive doubling_time_h = log(2) / mumax_percapita
    mu_rows <- combined[combined$metric_name == 'mumax_percapita', ]
    if (nrow(mu_rows) > 0) {
      dt_rows             <- mu_rows
      dt_rows$metric_name  <- 'doubling_time_h'
      dt_rows$metric_value <- ifelse(
        is.finite(mu_rows$metric_value) & mu_rows$metric_value > 0,
        log(2) / mu_rows$metric_value, NA_real_
      )
      dt_rows$metric_unit <- 'h'
      combined <- dplyr::bind_rows(combined, dt_rows)
    }

    # Restore full well_id from curve_id mapping
    id_map <- data.frame(
      gpfit_id = seq_len(length(unique(raw_long$well_id))),
      well_id  = sort(unique(raw_long$well_id)),
      stringsAsFactors = FALSE
    )
    combined$well_id   <- id_map$well_id[match(combined$well_id, id_map$gpfit_id)]
    combined$dataset_id <- unique(raw_long$dataset_id)[1]

    combined

  }, error = function(e) {
    message('[DGrowthR] Error: ', conditionMessage(e))
    data.frame()
  })

  list(
    processed_measurements = raw_long,
    derived_metrics        = metrics,
    model_fits             = data.frame(),
    logs = list(
      message  = if (nrow(metrics) > 0)
        paste('DGrowthR GPR: fitted', length(unique(metrics$well_id)), 'curves')
      else
        'DGrowthR fit failed; check messages above',
      citation = 'Feldl M & Olayo-Alarcon R. DGrowthR. https://bio-datascience.github.io/DGrowthR/',
      blog     = 'https://medinabajramovic.de/differential-growth-curve-analysis/',
      github   = 'https://github.com/bio-datascience/DGrowthR/'
    )
  )
}
