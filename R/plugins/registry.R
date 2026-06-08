QRES_PLUGINS <- data.frame(
  plugin      = c('model_free', 'gcplyr', 'growthrates', 'dgrothr', 'kinbiont', 'omniplate'),
  label       = c('Model-free (baseline)', 'gcplyr', 'growthrates', 'DGrowthR', 'KinBiont.jl', 'omniplate'),
  language    = c('R', 'R', 'R', 'R', 'Julia', 'Python'),
  url         = c(
    NA_character_,
    'https://github.com/melinabajramovic/gcplyr',
    'https://github.com/tpetzoldt/growthrates',
    'https://bio-datascience.github.io/DGrowthR/',
    'https://github.com/pinheiroGroup/Kinbiont.jl',
    'https://github.com/naef-lab/omniplate'
  ),
  citation    = c(
    'Qres DB built-in',
    'Blazanin M (2023). doi:10.1093/g3journal/jkad261',
    'Petzoldt T (2022). CRAN:growthrates',
    'Feldl M & Olayo-Alarcon R. DGrowthR. bio-datascience.github.io/DGrowthR/',
    'Pinheiro group. Kinbiont.jl',
    'naef-lab/omniplate'
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
    omniplate   = qres_plugin_omniplate,
    stop('Unknown plugin: ', plugin,
         '. Choose from: model_free, gcplyr, growthrates, dgrothr, kinbiont, omniplate')
  )
  fn(raw_long, metadata = metadata, params = params)
}

# ── Core workflow function 6 ──────────────────────────────────────────────────
# Run multiple plugins on the same raw_long data and return a unified
# long-format data frame suitable for side-by-side comparison.
qres_compare_plugin_outputs <- function(raw_long, plugins = c('model_free', 'gcplyr', 'growthrates'),
                                         params = list()) {
  requireNamespace('dplyr')
  results <- lapply(plugins, function(p) {
    res <- tryCatch(
      qres_run_plugin(raw_long, p, params),
      error = function(e) {
        message('[compare] Plugin ', p, ' failed: ', conditionMessage(e))
        list(derived_metrics = data.frame())
      }
    )
    if (nrow(res$derived_metrics) > 0) {
      dplyr::mutate(res$derived_metrics, tool = p)
    } else {
      NULL
    }
  })
  dplyr::bind_rows(Filter(Negate(is.null), results))
}
