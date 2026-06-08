# Embargo release checks.
#
# Three release modes:
#   'immediate' — released at upload time (no check needed)
#   'date'      — released when Sys.Date() >= release_date
#   'biorxiv'   — released when the bioRxiv preprint gains a journal DOI
#                 (checked monthly via the bioRxiv API)
#
# Run qres_run_embargo_check(con) once per month (or on app startup).
# Each check writes one row to embargo_check_log.

# Check whether a bioRxiv preprint URL/DOI has been formally published.
# Returns list(published = TRUE/FALSE, journal_doi = "..." or NA, error = NA/message).
qres_check_biorxiv_released <- function(biorxiv_url) {
  if (!requireNamespace('httr2', quietly = TRUE))
    return(list(published = FALSE, journal_doi = NA, error = 'httr2 not installed'))

  # Normalise: accept full URL or bare DOI (10.1101/...)
  doi <- sub('.*/(10\\.1101/[^?#]+).*', '\\1', biorxiv_url)
  doi <- sub('^https?://.*biorxiv\\.org/', '', doi)
  doi <- trimws(doi)

  api_url <- paste0('https://api.biorxiv.org/details/biorxiv/', doi, '/json')

  resp <- tryCatch(
    httr2::request(api_url) |>
      httr2::req_timeout(10) |>
      httr2::req_perform(),
    error = function(e) NULL
  )

  if (is.null(resp) || httr2::resp_status(resp) != 200)
    return(list(published = FALSE, journal_doi = NA,
                error = 'bioRxiv API request failed'))

  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(body) || length(body$collection) == 0)
    return(list(published = FALSE, journal_doi = NA,
                error = 'No record found in bioRxiv API'))

  # bioRxiv API: 'published' field = "NA" (string) if not published, else journal DOI
  pub_val <- body$collection[[length(body$collection)]]$published
  if (is.null(pub_val) || identical(pub_val, 'NA') || !nzchar(pub_val))
    return(list(published = FALSE, journal_doi = NA, error = NA))

  list(published = TRUE, journal_doi = pub_val, error = NA)
}

# Release a dataset: mark released = TRUE and record timestamp.
.qres_mark_released <- function(con, dataset_id, journal_doi = NA) {
  DBI::dbExecute(con, "
    UPDATE upload_embargo
    SET released = TRUE, released_at = ?
    WHERE dataset_id = ?
  ", params = list(format(Sys.time(), '%Y-%m-%d %H:%M:%S'), dataset_id))
  message(sprintf('[embargo] %s released (journal_doi: %s)',
                  dataset_id, journal_doi %||% 'N/A'))
}

# Run all pending release checks. Returns a data.frame of check results.
qres_run_embargo_check <- function(con) {
  pending <- DBI::dbGetQuery(con, "
    SELECT dataset_id, release_mode, release_date, biorxiv_url
    FROM upload_embargo
    WHERE released = FALSE
  ")

  if (nrow(pending) == 0) {
    message('[embargo] No unreleased datasets to check.')
    return(invisible(data.frame()))
  }

  results <- lapply(seq_len(nrow(pending)), function(i) {
    row         <- pending[i, ]
    dataset_id  <- row$dataset_id
    mode        <- row$release_mode
    check_id    <- paste0(dataset_id, '_', format(Sys.time(), '%Y%m%d%H%M%S'))
    result_txt  <- 'skipped'
    journal_doi <- NA
    released    <- FALSE

    if (mode == 'date') {
      if (!is.na(row$release_date) &&
          as.Date(row$release_date) <= Sys.Date()) {
        .qres_mark_released(con, dataset_id)
        result_txt <- 'released_by_date'
        released   <- TRUE
      } else {
        result_txt <- paste0('date_not_reached_', row$release_date)
      }

    } else if (mode == 'biorxiv') {
      if (!is.na(row$biorxiv_url) && nzchar(row$biorxiv_url %||% '')) {
        chk <- tryCatch(
          qres_check_biorxiv_released(row$biorxiv_url),
          error = function(e) list(published = FALSE, journal_doi = NA,
                                    error = conditionMessage(e))
        )
        if (isTRUE(chk$published)) {
          journal_doi <- chk$journal_doi
          .qres_mark_released(con, dataset_id, journal_doi)
          result_txt <- paste0('released_published:', journal_doi)
          released   <- TRUE
        } else {
          result_txt <- paste0('still_preprint',
                               if (!is.na(chk$error)) paste0('|error:', chk$error) else '')
        }
      } else {
        result_txt <- 'biorxiv_url_missing'
      }
    }

    # Write check log
    tryCatch(DBI::dbExecute(con, "
      INSERT INTO embargo_check_log (check_id, dataset_id, result, journal_doi)
      VALUES (?, ?, ?, ?)
    ", params = list(check_id, dataset_id, result_txt, journal_doi)),
    error = function(e) NULL)

    data.frame(dataset_id = dataset_id, mode = mode, result = result_txt,
               released = released, stringsAsFactors = FALSE)
  })

  do.call(rbind, results)
}

# Format next-check guidance for the UI.
qres_embargo_status_label <- function(rec) {
  if (is.null(rec) || nrow(rec) == 0) return('Status unknown')
  r <- rec[1, ]
  if (isTRUE(r$released))
    return(paste0('Public — released ', format(as.Date(r$released_at), '%Y-%m-%d')))
  mode <- r$release_mode
  if (mode == 'immediate') return('Public (immediate release)')
  if (mode == 'date')
    return(paste0('Embargoed — releases on ', r$release_date))
  if (mode == 'biorxiv')
    return(paste0('Embargoed — releases when bioRxiv preprint is published (',
                  if (!is.na(r$biorxiv_url)) substr(r$biorxiv_url, 1, 40) else 'URL missing', ')'))
  'Embargoed'
}
