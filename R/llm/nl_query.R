# Natural language → DuckDB query pipeline via the Anthropic Claude API.
#
# Architecture:
#   user question → Claude (text-to-SQL / action) → DuckDB or R functions → results
#
# Requires ANTHROPIC_API_KEY environment variable.

QRES_LLM_MODEL <- 'claude-sonnet-4-6'

.qres_llm_system_prompt <- function() {
  paste0(
    'You are an expert bioinformatics database assistant for Qres DB, ',
    'a database of microbial growth curves.\n\n',
    QRES_SCHEMA_CONTEXT, '\n\n',
    QRES_EXAMPLE_QUERIES, '\n\n',
    '## Instructions\n',
    'Given a user question, respond with a single JSON object (no markdown fences) with this structure:\n',
    '{\n',
    '  "action": "sql" | "compare_tools" | "info",\n',
    '  "sql": "SELECT ... (only for action=sql)",\n',
    '  "description": "Plain-English explanation of what the query does.",\n',
    '  "plot_type": null | "growth_curves" | "comparison_boxplot" | "comparison_scatter" | "table",\n',
    '  "compare_params": {\n',
    '    "species": null | "string",\n',
    '    "temp_min": null | number,\n',
    '    "temp_max": null | number,\n',
    '    "medium":   null | "string",\n',
    '    "plugins":  ["model_free", "gcplyr", "growthrates", ...],\n',
    '    "metric":   "mumax_percapita"\n',
    '  }\n',
    '}\n',
    'For action=info respond with just description (no sql or compare_params needed).\n',
    'Only use table/column names that exist in the schema above.\n',
    'Return valid DuckDB SQL only. No semicolons. No markdown.'
  )
}

qres_llm_available <- function() {
  api_key <- Sys.getenv('ANTHROPIC_API_KEY')
  nzchar(api_key)
}

qres_nl_to_action <- function(question) {
  api_key <- Sys.getenv('ANTHROPIC_API_KEY')
  if (!nzchar(api_key)) {
    return(list(
      action      = 'info',
      description = 'ANTHROPIC_API_KEY is not set. Set it via Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-…") or in your .Renviron file.',
      sql         = NULL,
      plot_type   = NULL
    ))
  }

  body <- list(
    model      = QRES_LLM_MODEL,
    max_tokens = 1024,
    system     = .qres_llm_system_prompt(),
    messages   = list(list(role = 'user', content = question))
  )

  resp <- tryCatch({
    req <- httr2::request('https://api.anthropic.com/v1/messages') |>
      httr2::req_headers(
        'x-api-key'         = api_key,
        'anthropic-version' = '2023-06-01',
        'content-type'      = 'application/json'
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
    httr2::resp_body_json(resp)
  }, error = function(e) {
    list(error = conditionMessage(e))
  })

  if (!is.null(resp$error)) {
    return(list(action = 'info',
                description = paste('API error:', resp$error),
                sql = NULL, plot_type = NULL))
  }

  raw_text <- tryCatch(resp$content[[1]]$text, error = function(e) '')
  parsed   <- tryCatch(jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
                        error = function(e) NULL)

  if (is.null(parsed)) {
    return(list(action = 'info',
                description = paste('LLM response could not be parsed:', raw_text),
                sql = NULL, plot_type = NULL))
  }

  parsed
}

# Execute the action returned by qres_nl_to_action() against the live DB.
# Returns list(type, data, description, plot_params)
qres_execute_action <- function(con, action_result, ...) {
  if (action_result$action == 'sql') {
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

  if (action_result$action == 'compare_tools') {
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
      return(list(type = 'table', data = data.frame(message = 'No datasets match filter.'),
                  description = action_result$description %||% '', plot_type = 'table'))
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

  # action == 'info'
  list(type = 'info', data = NULL,
       description = action_result$description %||% '',
       plot_type = NULL)
}
