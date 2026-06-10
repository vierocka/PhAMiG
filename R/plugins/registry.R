# Disk cache for expensive plugin results (DGrowthR, KinBiont).
# Keyed by digest of dataset ids + plugin name.  Valid 14 days, max 300 MB.
.PLUGIN_CACHE <- local({
  cache_dir <- file.path(getwd(), 'data', '.plugin_cache')
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  tryCatch(
    cachem::cache_disk(dir = cache_dir, max_size = 300 * 1024^2,
                       max_age = 14 * 24 * 3600),
    error = function(e) {
      message('[cache] Could not init disk cache: ', conditionMessage(e), ' — caching disabled.')
      cachem::cache_null()
    }
  )
})

.SLOW_PLUGINS <- c('dgrothr', 'kinbiont')

.run_plugin_maybe_cached <- function(raw_long, plugin, params = list(), metadata = NULL) {
  if (!plugin %in% .SLOW_PLUGINS || !requireNamespace('digest', quietly = TRUE)) {
    return(qres_run_plugin(raw_long, plugin, params, metadata))
  }
  key <- digest::digest(list(
    plugin = plugin,
    ids    = sort(unique(raw_long$dataset_id)),
    nrow   = nrow(raw_long),
    params = params
  ))
  if (.PLUGIN_CACHE$exists(key)) {
    message('[cache] Hit: ', plugin, ' on ',
            paste(sort(unique(raw_long$dataset_id)), collapse = ', '))
    return(.PLUGIN_CACHE$get(key))
  }
  result <- qres_run_plugin(raw_long, plugin, params, metadata)
  .PLUGIN_CACHE$set(key, result)
  result
}

QRES_PLUGINS <- data.frame(
  plugin      = c('model_free', 'gcplyr', 'growthrates', 'dgrothr', 'kinbiont'),
  label       = c('Model-free (baseline)', 'gcplyr', 'growthrates', 'DGrowthR', 'KinBiont.jl'),
  language    = c('R', 'R', 'R', 'R', 'Julia'),
  url         = c(
    NA_character_,
    'https://github.com/melinabajramovic/gcplyr',
    'https://github.com/tpetzoldt/growthrates',
    'https://bio-datascience.github.io/DGrowthR/',
    'https://github.com/pinheiroGroup/Kinbiont.jl'
  ),
  citation    = c(
    'Qres DB built-in',
    'Blazanin M (2023). doi:10.1093/g3journal/jkad261',
    'Petzoldt T (2022). CRAN:growthrates',
    'Feldl M & Olayo-Alarcon R. DGrowthR. bio-datascience.github.io/DGrowthR/',
    'Pinheiro group. Kinbiont.jl'
  ),
  stringsAsFactors = FALSE
)

qres_plugins <- function() QRES_PLUGINS

qres_new_processing_id <- function(dataset_id, plugin) {
  paste(dataset_id, plugin, format(Sys.time(), '%Y%m%d%H%M%S'),
        sample(1000:9999, 1), sep = '__')
}

qres_register_processing_run <- function(con, dataset_id, plugin,
                                          correction_type = 'none',
                                          params = list(),
                                          status = 'created',
                                          message = '') {
  requireNamespace('jsonlite')
  processing_id <- qres_new_processing_id(dataset_id, plugin)
  run <- data.frame(
    processing_id   = processing_id,
    dataset_id      = dataset_id,
    tool_name       = plugin,
    tool_version    = NA_character_,
    correction_type = correction_type,
    parameters_json = jsonlite::toJSON(params, auto_unbox = TRUE),
    container_hash  = NA_character_,
    git_commit      = NA_character_,
    status          = status,
    message         = message,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, 'processing_run', run, append = TRUE)
  processing_id
}

# ── Core workflow function 5 ──────────────────────────────────────────────────
# Dispatch to the named plugin and return the standard result list.
qres_run_plugin <- function(raw_long, plugin, params = list(), metadata = NULL) {
  fn <- switch(plugin,
    model_free  = qres_plugin_model_free,
    gcplyr      = qres_plugin_gcplyr,
    growthrates = qres_plugin_growthrates,
    dgrothr     = qres_plugin_dgrothr,
    kinbiont    = qres_plugin_kinbiont,
    stop('Unknown plugin: ', plugin,
         '. Choose from: model_free, gcplyr, growthrates, dgrothr, kinbiont')
  )
  fn(raw_long, metadata = metadata, params = params)
}

# ── Core workflow function 6 ──────────────────────────────────────────────────
# Run multiple plugins on the same raw_long data and return a unified
# long-format data frame suitable for side-by-side comparison.
qres_compare_plugin_outputs <- function(raw_long, plugins = c('model_free', 'gcplyr', 'growthrates'),
                                         params = list()) {
  requireNamespace('dplyr')
  plugin_errors <- character(0)
  results <- lapply(plugins, function(p) {
    res <- tryCatch(
      .run_plugin_maybe_cached(raw_long, p, params),
      error = function(e) {
        plugin_errors[[p]] <<- conditionMessage(e)
        list(derived_metrics = data.frame())
      }
    )
    if (!is.null(res$derived_metrics) && nrow(res$derived_metrics) > 0) {
      dplyr::mutate(res$derived_metrics, tool = p)
    } else {
      if (p %in% names(plugin_errors)) NULL
      else { plugin_errors[[p]] <<- 'no metrics returned'; NULL }
    }
  })
  df <- dplyr::bind_rows(Filter(Negate(is.null), results))
  attr(df, 'plugin_errors') <- plugin_errors
  df
}
