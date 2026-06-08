# Import dataset-level metadata CSV into DuckDB.
#
# REQUIRED columns (* in the CSV / UI):
#   dataset_id, raw_file, species, medium_type, temperature_c, perturbation_type
#
# OPTIONAL columns:
#   perturbation, strain, taxon_id, medium_state, concentration, concentration_unit,
#   plate_format, plate_reader, wavelength_nm, shake_freq_rpm, spectrophotometer,
#   time_unit, authors, year, doi, notes
#
# unique key: dataset_id (must be unique across the whole database)
# link to raw data: raw_file (CSV filename) → raw_measurement_registry.path (Parquet)
# link within DB: dataset_id → plate.dataset_id → well_metadata.plate_id → raw_measurement_registry.dataset_id

PHAMIG_REQUIRED_COLS <- c('dataset_id', 'raw_file', 'species', 'medium_type',
                           'temperature_c', 'perturbation_type')

PHAMIG_OPTIONAL_COLS <- c('perturbation', 'strain', 'taxon_id', 'medium_state',
                           'concentration', 'concentration_unit', 'plate_format',
                           'plate_reader', 'wavelength_nm', 'shake_freq_rpm',
                           'spectrophotometer', 'time_unit', 'authors', 'year', 'doi', 'notes')

PHAMIG_ALL_COLS <- c(PHAMIG_REQUIRED_COLS, PHAMIG_OPTIONAL_COLS)

qres_import_metadata <- function(con, metadata_csv = 'data/metaData.csv') {
  requireNamespace('readr'); requireNamespace('dplyr'); requireNamespace('DBI')

  md <- readr::read_csv(metadata_csv, show_col_types = FALSE,
                        col_types = readr::cols(.default = readr::col_character()))
  names(md) <- trimws(tolower(names(md)))

  # Validate column structure and field values
  val <- qres_validate_metadata(md)
  if (!val$ok)
    stop('Metadata validation failed:\n',
         paste0('  * ', val$issues, collapse = '\n'))
  if (length(val$warnings) > 0)
    warning('Metadata warnings:\n', paste0('  ⚠ ', val$warnings, collapse = '\n'), call. = FALSE)

  # Add any missing optional columns as NA
  for (col in setdiff(PHAMIG_ALL_COLS, names(md)))
    md[[col]] <- NA_character_

  md <- md |>
    dplyr::mutate(dplyr::across(dplyr::everything(), qres_clean_undefined)) |>
    dplyr::mutate(
      temperature_c    = qres_safe_numeric(temperature_c),
      concentration    = qres_safe_numeric(concentration),
      wavelength_nm    = qres_safe_numeric(wavelength_nm),
      shake_freq_rpm   = qres_safe_numeric(shake_freq_rpm),
      year             = suppressWarnings(as.integer(year)),
      # Auto-generate title if absent
      title = paste0(species, ' (', strain %||% species, ') — ',
                     medium_type, ' — ', temperature_c, '°C')
    )

  # ── dataset: one row per dataset_id ───────────────────────────────────────
  dataset <- md |>
    dplyr::transmute(
      dataset_id, raw_file, title,
      doi, year, authors, license = NA_character_,
      source_url = NA_character_, time_unit, notes
    ) |>
    dplyr::distinct(dataset_id, .keep_all = TRUE)

  # ── plate: instrument/run conditions ──────────────────────────────────────
  plate <- md |>
    dplyr::transmute(
      plate_id          = paste0(dataset_id, '__plate_1'),
      dataset_id,
      plate_format, plate_reader, spectrophotometer,
      wavelength_nm, temperature_c, shake_freq_rpm
    ) |>
    dplyr::distinct(plate_id, .keep_all = TRUE)

  # ── well_metadata: one representative row per plate ───────────────────────
  # Captures biological conditions for JOIN-based search.
  # A full per-well import uses qres_import_plate_template() with the plate template CSV.
  well_meta <- md |>
    dplyr::transmute(
      well_id            = paste0(dataset_id, '__plate_1__summary'),
      plate_id           = paste0(dataset_id, '__plate_1'),
      well               = 'summary',
      species, taxon_id, strain,
      medium_state, medium_type,
      perturbation_type,
      perturbation,
      concentration, concentration_unit,
      is_blank    = FALSE,
      is_control  = FALSE,
      replicate   = 1L
    ) |>
    dplyr::distinct(well_id, .keep_all = TRUE)

  DBI::dbWriteTable(con, 'dataset',       dataset,   append = TRUE)
  DBI::dbWriteTable(con, 'plate',         plate,     append = TRUE)
  DBI::dbWriteTable(con, 'well_metadata', well_meta, append = TRUE)

  message(sprintf('[import] %d datasets, %d plates, %d well summary rows written.',
                  nrow(dataset), nrow(plate), nrow(well_meta)))
  invisible(list(dataset = dataset, plate = plate, well_metadata = well_meta))
}

# Import a per-well plate template CSV (from Metadata Preparation page).
# Links to an existing dataset_id. Replaces the summary well row.
qres_import_plate_template <- function(con, template_csv, dataset_id) {
  requireNamespace('readr'); requireNamespace('DBI')

  tmpl <- readr::read_csv(template_csv, show_col_types = FALSE)
  names(tmpl) <- trimws(tolower(names(tmpl)))

  plate_id <- paste0(dataset_id, '__plate_1')

  # Remove old summary row for this plate
  DBI::dbExecute(con, 'DELETE FROM well_metadata WHERE plate_id = ? AND well = ?',
                 params = list(plate_id, 'summary'))

  # Build well_metadata rows
  rows <- dplyr::transmute(tmpl,
    well_id   = paste0(plate_id, '__', tolower(well)),
    plate_id  = plate_id,
    well,
    species            = dplyr::coalesce(species,            NA_character_),
    taxon_id           = dplyr::coalesce(taxon_id,           NA_character_),
    strain             = dplyr::coalesce(strain,             NA_character_),
    medium_state       = dplyr::coalesce(medium_state,       NA_character_),
    medium_type        = dplyr::coalesce(medium_type,        NA_character_),
    perturbation_type  = dplyr::coalesce(perturbation_type,  'none'),
    perturbation       = dplyr::coalesce(perturbation,       NA_character_),
    concentration      = suppressWarnings(as.numeric(dplyr::coalesce(concentration, NA_character_))),
    concentration_unit = dplyr::coalesce(concentration_unit, NA_character_),
    is_blank           = as.logical(dplyr::coalesce(is_blank,  'FALSE')),
    is_control         = as.logical(dplyr::coalesce(is_control,'FALSE')),
    replicate          = suppressWarnings(as.integer(dplyr::coalesce(replicate, '1')))
  )

  DBI::dbWriteTable(con, 'well_metadata', rows, append = TRUE)
  message(sprintf('[import] %d wells imported for %s.', nrow(rows), dataset_id))
  invisible(rows)
}
