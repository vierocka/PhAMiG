# Parse a wide plate-reader CSV (time × wells) into tidy long format,
# then store as Parquet (or fallback CSV) and register in raw_measurement_registry.
#
# Raw CSV format expected:
#   col 1 = time (name must contain "time", unit = hours by default)
#   cols 2..N = OD values, headers = well names (A1, A2, ..., H12)
#
# unique key chain:
#   dataset_id → raw_measurement_registry.dataset_id
#                raw_measurement_registry.path → Parquet/CSV file on disk
#
# Biological metadata (species, medium, perturbation…) is stored in well_metadata.
# well_metadata is populated by qres_import_metadata() (summary row) or
# qres_import_plate_template() (per-well rows). This file does NOT touch well_metadata.

qres_raw_to_long <- function(raw_csv, dataset_id,
                              plate_id    = paste0(dataset_id, '__plate_1'),
                              signal_type = 'OD600') {
  requireNamespace('data.table')
  dt       <- data.table::fread(raw_csv)
  time_col <- names(dt)[grepl('time', names(dt), ignore.case = TRUE)][1]
  if (is.na(time_col))
    stop('No column whose name contains "time" found in: ', raw_csv,
         '\n  Column 1 must be the time column.')

  long <- data.table::melt(dt, id.vars = time_col,
                            variable.name = 'well', value.name = 'value',
                            variable.factor = FALSE)
  data.table::setnames(long, time_col, 'time_h')
  long[, time_h := .parse_time_to_hours(as.character(time_h))]
  long[, dataset_id  := dataset_id]
  long[, plate_id    := plate_id]
  long[, well_id     := paste(dataset_id, plate_id, well, sep = '__')]
  long[, signal_type := signal_type]
  data.table::setcolorder(long, c('dataset_id', 'plate_id', 'well_id',
                                   'well', 'time_h', 'signal_type', 'value'))
  long[]
}

qres_register_raw_measurements <- function(con, dataset_id, raw_csv,
                                            out_dir      = 'data/parquet/raw_measurements',
                                            fallback_dir = 'data/long_csv/raw_measurements',
                                            signal_type  = 'OD600') {
  requireNamespace('DBI'); requireNamespace('data.table')

  # Validate raw CSV format before processing
  csv_check <- qres_validate_raw_csv(raw_csv)
  if (!csv_check$ok)
    stop('Raw CSV validation failed for ', basename(raw_csv), ':\n',
         paste0('  * ', csv_check$issues, collapse = '\n'))
  if (length(csv_check$warnings) > 0)
    warning('Raw CSV warnings for ', basename(raw_csv), ':\n',
            paste0('  ⚠ ', csv_check$warnings, collapse = '\n'), call. = FALSE)

  plate_id <- paste0(dataset_id, '__plate_1')
  long     <- qres_raw_to_long(raw_csv, dataset_id, plate_id, signal_type)

  # Write raw data to Parquet (preferred) or long CSV (fallback)
  if (requireNamespace('arrow', quietly = TRUE)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_path     <- file.path(out_dir, paste0(dataset_id, '.parquet'))
    arrow::write_parquet(as.data.frame(long), out_path)
    storage_type <- 'parquet'
  } else {
    dir.create(fallback_dir, recursive = TRUE, showWarnings = FALSE)
    out_path     <- file.path(fallback_dir, paste0(dataset_id, '_long.csv'))
    data.table::fwrite(long, out_path)
    storage_type <- 'csv_long'
  }

  # Register in DuckDB: path, shape, signal type
  registry <- data.frame(
    dataset_id   = dataset_id,
    storage_type = storage_type,
    path         = out_path,
    n_rows       = nrow(long),
    n_wells      = length(unique(long$well)),
    signal_type  = signal_type,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, 'raw_measurement_registry', registry, append = TRUE)

  message(sprintf('[register] %s: %d wells × %d rows → %s (%s)',
                  dataset_id, registry$n_wells, registry$n_rows, out_path, storage_type))
  invisible(registry)
}

# Read raw measurements back into memory as a long-format data frame.
# Returns: data.frame with columns dataset_id, plate_id, well_id, well, time_h, signal_type, value
qres_get_raw_curves <- function(con, dataset_id) {
  reg <- DBI::dbGetQuery(con,
    'SELECT * FROM raw_measurement_registry WHERE dataset_id = ?',
    params = list(dataset_id))
  if (nrow(reg) == 0)
    stop('No raw measurements registered for dataset_id: "', dataset_id, '".',
         '\n  Click "Initialize / refresh DB" or upload the raw CSV first.')
  .read_raw_file(reg$path[1])
}

# In-session memoised file reader — parquet files are immutable once written.
.read_raw_file <- memoise::memoise(function(path) {
  if (grepl('\\.parquet$', path) && requireNamespace('arrow', quietly = TRUE)) {
    as.data.frame(arrow::read_parquet(path))
  } else {
    as.data.frame(data.table::fread(path))
  }
})

qres_read_raw_measurements <- qres_get_raw_curves
