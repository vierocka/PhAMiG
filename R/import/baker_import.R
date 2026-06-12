# R/import/baker_import.R
# ──────────────────────────────────────────────────────────────────────────────
# Sao Emani C, Williams MJ, Van Helden PD, Taylor MJC, Carolis C, Wiid IJ,
# Baker B (2018). Generation and characterization of thiol-deficient
# Mycobacterium tuberculosis mutants. Scientific Data 5:180184.
# DOI: 10.1038/sdata.2018.184
#
# PHENOTYPE-RESCUE VALIDATION DATASET
# This file contains growth curves for genetic COMPLEMENT strains only:
#   CDC1551 (wild-type reference) vs ΔegtAc / ΔegtBc / ΔegtCc / ΔegtDc / ΔmshAc
# Purpose: complements re-introduce the functional gene into the knockout and
# should restore wild-type growth — validating that the knockout phenotype is
# specifically due to loss of that gene.
# The corresponding knockout data (ΔegtA…E, ΔmshA) is in Metabolights (study
# MTBLS541) and is NOT included here.
#
# Growth conditions:
#   OD₆₀₀, manual spectrophotometer | 37°C, static (no shaking)
#   Middlebrook 7H9 + OADC | 75 cm² vented flasks, double-sealed plastic bags
#   Starting OD₆₀₀ = 0.05 | Measured every 2 days, 0–24 days
#   3 biological replicates per strain | 18 curves total
#
# Usage:
#   source('R/import/baker_import.R')
#   baker_import_complements(con)
# ──────────────────────────────────────────────────────────────────────────────

BAKER_DOI     <- '10.1038/sdata.2018.184'
BAKER_YEAR    <- 2018L
BAKER_AUTHORS <- 'Sao Emani C, Williams MJ, Van Helden PD, Taylor MJC, Carolis C, Wiid IJ, Baker B'
BAKER_LICENSE <- 'CC BY 4.0'
BAKER_DATASET <- 'baker_2018_mtb_gc'

baker_import_complements <- function(
    con,
    source_dir  = '/home/veve/Dropbox/GrowthDB_Qres/Mycobacterium_tuberculosis_mutants_2018_Baker',
    parquet_dir = 'data/parquet/raw_measurements',
    overwrite   = FALSE,
    verbose     = TRUE) {

  .msg <- function(...) if (verbose) message(...)

  did      <- BAKER_DATASET
  plate_id <- paste0(did, '__plate_1')
  pq_path  <- file.path(parquet_dir, paste0(did, '.parquet'))

  # ── Schema migration (idempotent) ──────────────────────────────────────────
  tryCatch(DBI::dbExecute(con,
    "ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS dataset_id TEXT"),
    error = function(e) NULL)

  # ── Overwrite guard ────────────────────────────────────────────────────────
  existing <- DBI::dbGetQuery(con,
    "SELECT dataset_id FROM dataset WHERE dataset_id = ?", params = list(did))
  if (nrow(existing) > 0L) {
    if (!overwrite) {
      .msg('Dataset ', did, ' already exists. Use overwrite=TRUE to re-import.')
      return(invisible(NULL))
    }
    .msg('Removing existing ', did, ' ...')
    for (tbl in c('derived_metric', 'model_fit', 'raw_measurement_registry', 'dataset'))
      DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE dataset_id = ?", tbl),
                     params = list(did))
    DBI::dbExecute(con, "DELETE FROM well_metadata WHERE plate_id = ?",
                   params = list(plate_id))
    DBI::dbExecute(con, "DELETE FROM plate WHERE plate_id = ?",
                   params = list(plate_id))
    if (file.exists(pq_path)) file.remove(pq_path)
  }

  # ── Read XLSX ──────────────────────────────────────────────────────────────
  xlsx_path <- file.path(source_dir, 'Geneticcomplementsgrowthcurve.xlsx')
  stopifnot(file.exists(xlsx_path))

  raw <- suppressMessages(
    readxl::read_xlsx(xlsx_path, col_names = FALSE, .name_repair = 'minimal'))

  # Row 1 = column headers: "days", "CDC1551", NA, NA, "egtAc", NA, NA, ...
  #  = Δ rendered via Symbol-font private-use glyph in the original file
  hdr  <- gsub('', 'Δ', as.character(unlist(raw[1, ])), fixed = TRUE)
  data <- as.data.frame(lapply(raw[-1, ], as.numeric))

  # ── Strain / well mapping ──────────────────────────────────────────────────
  # Strain names appear at columns 2, 5, 8, 11, 14, 17 (every 3rd starting at 2)
  strain_cols  <- seq(2L, ncol(data), by = 3L)
  strain_names <- hdr[strain_cols]   # CDC1551, ΔegtAc, ΔegtBc, ΔegtCc, ΔegtDc, ΔmshAc
  well_letters <- LETTERS[seq_along(strain_cols)]

  col_meta <- data.frame(
    col_idx = seq_len(ncol(data))[-1L],         # skip col 1 (time)
    strain  = rep(strain_names, each = 3L),
    well    = unlist(lapply(well_letters, function(r) paste0(r, 1:3))),
    rep     = rep(1:3, times = length(strain_names)),
    stringsAsFactors = FALSE
  )

  # ── Reshape to long format ─────────────────────────────────────────────────
  # Original time unit is days; convert to hours for consistency with other datasets
  time_h <- data[[1L]] * 24

  long <- do.call(rbind, lapply(seq_len(nrow(col_meta)), function(i) {
    data.frame(
      well_id = paste0(plate_id, '__', tolower(col_meta$well[i])),
      well    = col_meta$well[i],
      time_h  = time_h,
      value   = data[[ col_meta$col_idx[i] ]],
      stringsAsFactors = FALSE
    )
  }))
  long <- long[!is.na(long$value), ]

  # ── Write Parquet ──────────────────────────────────────────────────────────
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(long, pq_path)
  .msg('Parquet written: ', nrow(long), ' rows -> ', pq_path)

  # ── dataset ────────────────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    INSERT INTO dataset
      (dataset_id, raw_file, title, doi, year, authors, license, source_url,
       time_unit, notes)
    VALUES (?,?,?,?,?,?,?,?,?,?)",
    params = list(
      did,
      basename(xlsx_path),
      'M. tuberculosis CDC1551 thiol-deficient mutant genetic complements',
      BAKER_DOI,
      BAKER_YEAR,
      BAKER_AUTHORS,
      BAKER_LICENSE,
      paste0('https://doi.org/', BAKER_DOI),
      'hours',
      paste0(
        'PHENOTYPE-RESCUE VALIDATION: growth curves for genetic complement strains ',
        '(knockout mutant + re-introduced functional gene copy). Complements are ',
        'expected to restore wild-type (CDC1551) growth, confirming that any ',
        'phenotype in the knockout is specifically caused by loss of that gene. ',
        'Ergothioneine (ERG) pathway complements: ΔegtAc, ΔegtBc, ',
        'ΔegtCc, ΔegtDc. Mycothiol (MSH) pathway complement: ΔmshAc. ',
        'Corresponding knockout data (ΔegtA–E, ΔmshA) is deposited ',
        'in Metabolights (study MTBLS541) and not included here. ',
        'Medium: Middlebrook 7H9 + OADC. Static culture (no shaking), 37°C, ',
        'double-sealed plastic bags (BSL-3). OD₆₀₀ measured every ',
        '2 days, 0–24 days, 3 biological replicates per strain.'
      )
    )
  )

  # ── plate ──────────────────────────────────────────────────────────────────
  DBI::dbExecute(con, "
    INSERT INTO plate
      (plate_id, dataset_id, plate_format, spectrophotometer, temperature_c,
       shake_freq_rpm)
    VALUES (?,?,?,?,?,?)",
    params = list(plate_id, did,
                  '75 cm² vented tissue culture flask',
                  'manual spectrophotometer',
                  37.0, 0.0)
  )

  # ── well_metadata ──────────────────────────────────────────────────────────
  wt_strain <- 'CDC1551'

  for (i in seq_len(nrow(col_meta))) {
    strain  <- col_meta$strain[i]
    well    <- col_meta$well[i]
    well_id <- paste0(plate_id, '__', tolower(well))
    is_wt   <- (strain == wt_strain)

    if (is_wt) {
      pert_type  <- 'none'
      perturbation <- NA_character_
    } else {
      # 'ΔegtAc' -> gene = 'egtA', perturbation = 'ΔegtA complement'
      gene       <- sub('^[Δ](.+)c$', '\\1', strain)
      perturbation <- paste0('Δ', gene, ' complement')
      pert_type  <- 'genetic'
    }

    DBI::dbExecute(con, "
      INSERT INTO well_metadata
        (well_id, plate_id, dataset_id, well, species, strain,
         medium_state, medium_type,
         perturbation_type, perturbation,
         is_blank, is_control, replicate)
      VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)",
      params = list(
        well_id, plate_id, did, well,
        'M. tuberculosis', strain,
        'liquid', 'Middlebrook 7H9 + OADC',
        pert_type, perturbation,
        FALSE, is_wt,
        col_meta$rep[i]
      )
    )
  }

  # ── raw_measurement_registry ───────────────────────────────────────────────
  DBI::dbExecute(con, "
    INSERT INTO raw_measurement_registry
      (dataset_id, storage_type, path, n_rows, n_wells, signal_type)
    VALUES (?,?,?,?,?,?)",
    params = list(did, 'parquet', pq_path,
                  nrow(long), length(unique(long$well_id)), 'OD600')
  )

  .msg('Done. dataset_id: ', did,
       ' | strains: ', length(strain_names),
       ' | wells: ', nrow(col_meta),
       ' | time points: ', length(time_h),
       ' | rows: ', nrow(long))
  invisible(did)
}
