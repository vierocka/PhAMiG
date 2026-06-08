# LLM-backed NL → filter extraction for PhaMiG.
# Uses ellmer package: tries Anthropic first (if ANTHROPIC_API_KEY set),
# then ollama (if running locally). Falls back to rule-based parser silently.
#
# mall is also available (loaded in app.R) for future batch column operations.

.PHAMIG_NL_SYSTEM <- paste(
  'You are a database assistant for PhaMiG, a microbial growth curve database.',
  'Extract structured search filters from a plain-English query.',
  'Return ONLY valid JSON with these optional keys:',
  '  species (string), strain (string), medium (string),',
  '  temp_min (number), temp_max (number),',
  '  perturbation_type ("chemical"|"genetic"|"temperature"|"none"),',
  '  perturbation (string), author (string), year_min (int), year_max (int).',
  'If a key cannot be inferred, omit it entirely.',
  'Examples:',
  '  Query: "E. coli at 35-37C in LB" ->',
  '    {"species":"E. coli","medium":"LB","temp_min":35,"temp_max":37}',
  '  Query: "yeast rifampicin dose response 2023" ->',
  '    {"species":"S. cerevisiae","perturbation":"rifampicin",',
  '     "perturbation_type":"chemical","year_min":2023,"year_max":2023}',
  'Return ONLY the JSON object. No explanation, no markdown fences.'
)

# Detect which LLM backend is available.
# Returns: "anthropic", "ollama", or "none"
qres_llm_backend <- function() {
  if (!requireNamespace('ellmer', quietly = TRUE)) return('none')

  if (nzchar(Sys.getenv('ANTHROPIC_API_KEY'))) return('anthropic')

  # Check if ollama is reachable on the default port
  ollama_ok <- tryCatch({
    requireNamespace('httr2', quietly = TRUE) &&
    httr2::request('http://localhost:11434/api/tags') |>
      httr2::req_timeout(2) |>
      httr2::req_perform() |>
      httr2::resp_status() == 200
  }, error = function(e) FALSE)

  if (isTRUE(ollama_ok)) return('ollama')
  'none'
}

# Cache backend detection per session
.LLM_BACKEND_CACHE <- new.env(parent = emptyenv())
.LLM_BACKEND_CACHE$backend <- NA_character_

qres_llm_available <- function() {
  # Only cache positive results — recheck if previously "none" so a key added
  # to .Renviron and a session restart is picked up without clearing state.
  if (is.na(.LLM_BACKEND_CACHE$backend) || .LLM_BACKEND_CACHE$backend == 'none')
    .LLM_BACKEND_CACHE$backend <- qres_llm_backend()
  .LLM_BACKEND_CACHE$backend != 'none'
}

qres_llm_backend_name <- function() {
  if (is.na(.LLM_BACKEND_CACHE$backend) || .LLM_BACKEND_CACHE$backend == 'none')
    .LLM_BACKEND_CACHE$backend <- qres_llm_backend()
  .LLM_BACKEND_CACHE$backend
}

# Build an ellmer chat object for the active backend.
.qres_make_chat <- function() {
  backend <- qres_llm_backend_name()
  if (backend == 'anthropic') {
    ellmer::chat_anthropic(
      system_prompt = .PHAMIG_NL_SYSTEM,
      model         = 'claude-haiku-4-5-20251001'
    )
  } else if (backend == 'ollama') {
    for (model in c('llama3.2', 'llama3', 'gemma2', 'mistral', 'phi3', 'qwen2.5')) {
      chat <- tryCatch(
        ellmer::chat_ollama(model = model, system_prompt = .PHAMIG_NL_SYSTEM),
        error = function(e) NULL
      )
      if (!is.null(chat)) return(chat)
    }
    NULL
  } else {
    NULL
  }
}

# Parse NL query via LLM. Returns named list of filters compatible with
# qres_filter_datasets(), or NULL on failure.
qres_nl_to_action <- function(text) {
  chat <- tryCatch(.qres_make_chat(), error = function(e) NULL)
  if (is.null(chat)) return(NULL)

  raw <- tryCatch(chat$chat(text), error = function(e) NULL)
  if (is.null(raw) || !nzchar(trimws(raw %||% ''))) return(NULL)

  # Strip markdown code fences if present
  json_str <- gsub('^```(?:json)?\\s*|\\s*```$', '', trimws(raw), perl = TRUE)

  filters <- tryCatch(jsonlite::fromJSON(json_str, simplifyVector = TRUE),
                      error = function(e) NULL)
  if (is.null(filters) || !is.list(filters)) return(NULL)

  # Coerce numeric fields
  for (f in c('temp_min', 'temp_max', 'year_min', 'year_max'))
    if (!is.null(filters[[f]])) filters[[f]] <- suppressWarnings(as.numeric(filters[[f]]))

  filters
}

# Execute the action returned by qres_nl_to_action() against the live DB.
# Returns list(type, data, description, plot_params)
qres_execute_action <- function(con, action_result, ...) {
  if (!is.null(action_result$sql) && nzchar(action_result$sql %||% '')) {
    sql <- action_result$sql
    df  <- tryCatch(
      DBI::dbGetQuery(con, sql),
      error = function(e) data.frame(error = conditionMessage(e))
    )
    return(list(
      type        = 'table',
      data        = df,
      description = action_result$description %||% '',
      sql         = sql,
      plot_type   = action_result$plot_type %||% 'table'
    ))
  }

  if (!is.null(action_result$compare_params)) {
    cp      <- action_result$compare_params %||% list()
    plugins <- unlist(cp$plugins) %||% c('model_free', 'gcplyr', 'growthrates')
    metric  <- cp$metric %||% 'mumax_percapita'

    filtered <- qres_filter_datasets(
      con,
      species    = cp$species,
      temp_min   = cp$temp_min,
      temp_max   = cp$temp_max,
      medium     = cp$medium,
      registered_only = TRUE
    )

    if (nrow(filtered) == 0) {
      return(list(type = 'table',
                  data = data.frame(message = 'No datasets match filter.'),
                  description = action_result$description %||% '',
                  plot_type = 'table'))
    }

    all_metrics <- lapply(filtered$dataset_id, function(did) {
      raw <- tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
      if (is.null(raw)) return(NULL)
      cmp <- qres_compare_plugin_outputs(raw, plugins = plugins)
      if (nrow(cmp) > 0) {
        cmp$dataset_id    <- did
        cmp$temperature_c <- filtered$temperature_c[filtered$dataset_id == did]
      }
      cmp
    })

    combined <- dplyr::bind_rows(Filter(Negate(is.null), all_metrics))
    return(list(
      type        = 'comparison',
      data        = combined,
      description = action_result$description %||% '',
      plot_type   = action_result$plot_type %||% 'comparison_boxplot',
      metric      = metric,
      filtered    = filtered
    ))
  }

  list(type = 'info', data = NULL,
       description = action_result$description %||% '',
       plot_type   = NULL)
}
