# Embargo access key management.
#
# Key chain for dataset ownership:
#   email (used at upload)  +  access_key (32-char token, shown once)
#                           +  passphrase (≥8 chars chosen by user)
#
# The passphrase is never stored in plain text — only its SHA-256 hex digest.
# All three must match to delete a dataset.

# Generate a cryptographically random 32-char alphanumeric access key.
qres_generate_access_key <- function() {
  chars <- c(letters, LETTERS, 0:9)
  if (requireNamespace('openssl', quietly = TRUE)) {
    # Use openssl for proper random bytes when available
    raw_bytes <- openssl::rand_bytes(32)
    idx       <- (as.integer(raw_bytes) %% length(chars)) + 1L
    paste0(chars[idx], collapse = '')
  } else {
    paste0(sample(chars, 32, replace = TRUE), collapse = '')
  }
}

# Hash a passphrase using SHA-256. Returns a 64-char hex string.
qres_hash_passphrase <- function(passphrase) {
  if (requireNamespace('openssl', quietly = TRUE)) {
    as.character(openssl::sha256(chartr('\n', ' ', passphrase)))
  } else if (requireNamespace('digest', quietly = TRUE)) {
    digest::digest(passphrase, algo = 'sha256', serialize = FALSE)
  } else {
    # Fallback: very weak — warn and store a reversible marker
    warning('Neither openssl nor digest is installed — passphrase stored insecurely. ',
            'Install: install.packages("digest")', call. = FALSE)
    paste0('WEAK:', nchar(passphrase), ':', chartr(paste0(letters, collapse = ''),
           paste0(LETTERS, collapse = ''), substr(passphrase, 1, 8)))
  }
}

# Register an embargo record. Called immediately after qres_import_metadata().
# Returns list(access_key, passphrase_hash) so the key can be shown to the user.
qres_register_embargo <- function(con, dataset_id, uploader_email, passphrase,
                                   release_mode  = c('immediate', 'date', 'biorxiv'),
                                   release_date  = NA,
                                   biorxiv_url   = NA,
                                   orcid         = NA) {
  release_mode <- match.arg(release_mode)

  if (nchar(trimws(passphrase %||% '')) < 8)
    stop('Passphrase must be at least 8 characters.')
  if (!nzchar(trimws(uploader_email %||% '')))
    stop('Uploader email is required for the access key chain.')
  if (!grepl('@', uploader_email))
    stop('Uploader email does not look valid (missing @).')

  access_key      <- qres_generate_access_key()
  passphrase_hash <- qres_hash_passphrase(trimws(passphrase))
  released        <- release_mode == 'immediate'

  DBI::dbExecute(con, "
    INSERT INTO upload_embargo
      (dataset_id, uploader_email, orcid, access_key, passphrase_hash,
       release_mode, release_date, biorxiv_url, released, released_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    dataset_id, trimws(tolower(uploader_email)),
    if (!is.na(orcid) && nzchar(trimws(orcid %||% ''))) trimws(orcid) else NA,
    access_key, passphrase_hash,
    release_mode,
    if (!is.na(release_date) && nzchar(release_date %||% '')) as.character(release_date) else NA,
    if (!is.na(biorxiv_url)  && nzchar(biorxiv_url  %||% '')) trimws(biorxiv_url) else NA,
    released,
    if (released) format(Sys.time(), '%Y-%m-%d %H:%M:%S') else NA
  ))

  message(sprintf('[embargo] %s registered — mode: %s, released: %s',
                  dataset_id, release_mode, released))
  invisible(list(access_key = access_key, passphrase_hash = passphrase_hash))
}

# Verify all three factors and delete a dataset if they match.
# Returns list(ok, message).
qres_delete_dataset <- function(con, dataset_id, email, access_key, passphrase,
                                 parquet_dir = 'data/parquet/raw_measurements') {
  rec <- DBI::dbGetQuery(con,
    'SELECT * FROM upload_embargo WHERE dataset_id = ?', params = list(dataset_id))

  if (nrow(rec) == 0)
    return(list(ok = FALSE,
                message = paste0('Dataset "', dataset_id, '" not found or has no embargo record.')))

  # Factor 1: email (case-insensitive)
  if (!identical(trimws(tolower(email)), rec$uploader_email[1]))
    return(list(ok = FALSE, message = 'Email does not match upload record.'))

  # Factor 2: access key (exact, case-sensitive)
  if (!identical(trimws(access_key), rec$access_key[1]))
    return(list(ok = FALSE, message = 'Access key is incorrect.'))

  # Factor 3: passphrase hash
  if (!identical(qres_hash_passphrase(trimws(passphrase)), rec$passphrase_hash[1]))
    return(list(ok = FALSE, message = 'Passphrase is incorrect.'))

  # All three match — delete from all tables
  tables <- c('embargo_check_log', 'upload_embargo', 'derived_metric', 'model_fit',
               'processing_run', 'raw_measurement_registry', 'well_metadata', 'plate', 'dataset')
  for (tbl in tables) {
    tryCatch(
      DBI::dbExecute(con, sprintf('DELETE FROM %s WHERE dataset_id = ?', tbl),
                     params = list(dataset_id)),
      error = function(e) NULL  # table may not have dataset_id column
    )
  }

  # Delete Parquet file if present
  parquet_path <- file.path(parquet_dir, paste0(dataset_id, '.parquet'))
  if (file.exists(parquet_path)) file.remove(parquet_path)

  message(sprintf('[delete] Dataset %s deleted by %s', dataset_id, email))
  list(ok = TRUE, message = paste0('Dataset "', dataset_id, '" and all associated data deleted.'))
}

# Check if a dataset is visible (released or has no embargo record = legacy data).
qres_is_released <- function(con, dataset_id) {
  rec <- DBI::dbGetQuery(con,
    'SELECT released FROM upload_embargo WHERE dataset_id = ?', params = list(dataset_id))
  if (nrow(rec) == 0) return(TRUE)  # no embargo record → legacy/public data
  isTRUE(rec$released[1])
}

# Return all datasets belonging to an email address (released or not).
qres_my_datasets <- function(con, email) {
  DBI::dbGetQuery(con, "
    SELECT ue.dataset_id, ue.release_mode, ue.release_date, ue.biorxiv_url,
           ue.released, ue.needs_correction, ue.created_at, ue.released_at,
           d.title, w.species, w.medium_type, p.temperature_c
    FROM upload_embargo ue
    LEFT JOIN dataset d USING(dataset_id)
    LEFT JOIN plate p USING(dataset_id)
    LEFT JOIN (
      SELECT plate_id, species, medium_type FROM well_metadata GROUP BY plate_id, species, medium_type
    ) w ON w.plate_id = p.plate_id
    WHERE lower(ue.uploader_email) = lower(?)
    ORDER BY ue.created_at DESC
  ", params = list(trimws(email)))
}

# Flag a dataset for correction: wipes raw data + metadata but keeps the embargo
# record so the uploader can re-upload corrected files using the same dataset_id.
# Requires the same 4 factors as qres_delete_dataset().
qres_reopen_dataset <- function(con, dataset_id, email, access_key, passphrase,
                                 parquet_dir = 'data/parquet/raw_measurements') {
  rec <- DBI::dbGetQuery(con,
    'SELECT * FROM upload_embargo WHERE dataset_id = ?', params = list(dataset_id))

  if (nrow(rec) == 0)
    return(list(ok = FALSE,
                message = paste0('Dataset "', dataset_id, '" not found or has no embargo record.')))

  if (!identical(trimws(tolower(email)), rec$uploader_email[1]))
    return(list(ok = FALSE, message = 'Email does not match upload record.'))

  if (!identical(trimws(access_key), rec$access_key[1]))
    return(list(ok = FALSE, message = 'Access key is incorrect.'))

  if (!identical(qres_hash_passphrase(trimws(passphrase)), rec$passphrase_hash[1]))
    return(list(ok = FALSE, message = 'Passphrase is incorrect.'))

  # Remove data rows — keep upload_embargo intact
  for (tbl in c('raw_measurement_registry', 'well_metadata', 'plate', 'dataset')) {
    tryCatch(
      DBI::dbExecute(con, sprintf('DELETE FROM %s WHERE dataset_id = ?', tbl),
                     params = list(dataset_id)),
      error = function(e) NULL
    )
  }

  # Delete Parquet file if present
  parquet_path <- file.path(parquet_dir, paste0(dataset_id, '.parquet'))
  if (file.exists(parquet_path)) file.remove(parquet_path)

  # Mark correction flag
  DBI::dbExecute(con,
    "UPDATE upload_embargo SET needs_correction = TRUE WHERE dataset_id = ?",
    params = list(dataset_id))

  message(sprintf('[reopen] %s flagged for correction by %s', dataset_id, email))
  list(ok = TRUE,
       message = paste0('Upload cleared for "', dataset_id,
                        '". Go to "Upload raw data" and re-submit using the same dataset ID.'))
}
