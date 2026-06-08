# Validate a metaData.csv file against current PhaMiG column names and field limits.
# Thin wrapper around qres_validate_metadata() from input_checks.R.

qres_check_metadata <- function(metadata_csv = 'data/metaData.csv') {
  if (!file.exists(metadata_csv))
    return(list(ok = FALSE,
                issues = paste('File not found:', metadata_csv),
                warnings = character(0)))

  md <- tryCatch(
    readr::read_csv(metadata_csv, show_col_types = FALSE,
                    col_types = readr::cols(.default = readr::col_character())),
    error = function(e) NULL
  )
  if (is.null(md))
    return(list(ok = FALSE,
                issues = paste('Cannot parse CSV:', metadata_csv),
                warnings = character(0)))

  names(md) <- trimws(tolower(names(md)))

  # Detect stale column names from old format and give a helpful message
  stale_names <- intersect(names(md), c('Stamp', 'Species', 'Temperature', 'MediumType', 'File'))
  if (length(stale_names) > 0)
    return(list(ok = FALSE,
                issues = paste0(
                  'CSV uses old column names (', paste(stale_names, collapse = ', '), '). ',
                  'Rename to snake_case: dataset_id, species, temperature_c, medium_type, raw_file'),
                warnings = character(0)))

  # Warn about missing raw files
  file_warns <- character(0)
  if ('raw_file' %in% names(md)) {
    missing_files <- md$raw_file[!file.exists(file.path('data/raw_csv', md$raw_file))]
    missing_files <- missing_files[!is.na(missing_files)]
    if (length(missing_files) > 0)
      file_warns <- paste('Raw CSV not found in data/raw_csv/:', paste(missing_files, collapse = ', '))
  }

  result <- qres_validate_metadata(md)
  result$warnings <- c(result$warnings, file_warns)
  result
}
