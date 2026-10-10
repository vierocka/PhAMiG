# R/import/zhang_import.R
# ──────────────────────────────────────────────────────────────────────────────
# Batch import: Zhang, Sawamura & Ying (2026) six bacteria × defined media
# Zhang S, Sawamura K, Ying BW (2026) Scientific Data
# DOI: 10.1038/s41597-026-07990-x  |  Figshare: 31239436 (CC BY 4.0)
#
# 11,686 raw OD600 curves (30 min interval, 0–48 h) in three assays:
#   I   (LE_*)  E. coli + L. plantarum,       196 media, 11 components
#   II  (SI_*)  all six species,              251 media, 30 components
#   III (SW_*)  E. coli, S. arlettae, Y. berc. 217 media, 56 components
#
# Design: 1 species × 1 medium = 1 dataset (≈2,550 datasets); each dataset holds
#   that medium's replicate curves (usually 4). well = author curve ID (curves of
#   one medium come from different plates, so plate well labels are not unique).
#
# Values are the raw OD600 records as deposited — not background-subtracted
# (the authors subtract the t = 0 value only for their own parameter estimation).
# 27 assay-II curves contain negative readings after a sudden drop (plate-reader
# artefact); they are kept unchanged and listed in the dataset notes.
#
# Assay II matches the protocol of Zhang & Ying (2025) Sci Rep 15:32969
# (same six strains, 30 components): Epoch2, 96-well, 32 °C, 567 rpm.
# Temperature and instrument for assays I and III are not reported in the data
# deposit, and the article methods were not yet available → left NULL.
#
# Author metrics (Info_*.xlsx): K (a.u.), r (1/h), t = lag time (h); 0 = no growth.
#
# Usage:
#   source('R/import/zhang_import.R')
#   zhang_import_all(con, source_dir = '/path/to/Zhang_2026_six_bacteria_CDM')
# ──────────────────────────────────────────────────────────────────────────────

ZHANG_DOI        <- '10.1038/s41597-026-07990-x'
ZHANG_YEAR       <- 2026L
ZHANG_AUTHORS    <- 'Zhang S, Sawamura K, Ying BW'
ZHANG_LICENSE    <- 'CC BY 4.0'
ZHANG_SOURCE_URL <- 'https://doi.org/10.6084/m9.figshare.31239436'

# Abbreviation → species name, strain, NCBI taxon ID
# (strains: Zhang & Ying 2025 Sci Rep, doi 10.1038/s41598-025-17103-0)
ZHANG_SPECIES <- data.frame(
  abbr     = c('Ec', 'Lp', 'Yb', 'Bs', 'As', 'Sa'),
  species  = c('E. coli', 'Lactiplantibacillus plantarum', 'Yersinia bercovieri',
               'Brevundimonas subvibrioides', 'Aeromonas salmonicida subsp. masoucida',
               'Staphylococcus arlettae'),
  strain   = c('K-12 BW25113', 'ATCC 8014', 'ATCC 43970', 'ATCC 15264', 'ATCC 27013',
               'ATCC 43957'),
  taxon_id = c('562', '1590', '634', '74313', '197700', '29378'),
  stringsAsFactors = FALSE)

ZHANG_ASSAYS <- list(
  I   = list(desc = '11 components (tryptone/yeast extract base)',
             temperature_c = NA_real_, plate_reader = NA_character_,
             shake_rpm = NA_real_, plate_format = NA_character_),
  II  = list(desc = '30 components (natural ingredients + pure chemicals)',
             temperature_c = 32, plate_reader = 'EPOCH2 (BioTek)',
             shake_rpm = 567, plate_format = '96'),
  III = list(desc = '56 components (amino acids, vitamins, salts)',
             temperature_c = NA_real_, plate_reader = NA_character_,
             shake_rpm = NA_real_, plate_format = '96'))

# Medium IDs in Info_*.xlsx that do not match Media_*.xlsx.
# 'LE_c0009' (8 curves) sits between LE_c00008 and LE_c00010 and LE_c00009 is the
# only medium in Media_I.xlsx used by no curve → a 4-digit typo for LE_c00009.
ZHANG_MEDIUM_FIX <- c(LE_c0009 = 'LE_c00009')

zhang_migrate_schema <- function(con) {
  for (sql in c(
    "ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS dataset_id TEXT",
    "ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS gene_category TEXT",
    "ALTER TABLE medium_composition ADD COLUMN IF NOT EXISTS conc_value DOUBLE",
    "ALTER TABLE medium_composition ADD COLUMN IF NOT EXISTS conc_unit TEXT"
  )) tryCatch(DBI::dbExecute(con, sql), error = function(e) NULL)
  invisible(TRUE)
}

# Read one assay (I / II / III) into curves (wide), info and media tables
.zhang_read_assay <- function(source_dir, assay) {
  rd <- function(kind) suppressMessages(as.data.frame(readxl::read_xlsx(
    file.path(source_dir, sprintf('%s_%s.xlsx', kind, assay)), sheet = 1)))
  curves <- rd('Curves'); info <- rd('Info'); media <- rd('Media')
  names(curves)[1] <- 'time_h'
  names(info)[1:3] <- c('curve_id', 'medium_id', 'abbr')
  names(info)[names(info) == 'Well location'] <- 'well_location'
  names(info)[grepl('^K', names(info))]       <- 'K'
  names(info)[grepl('^r ', names(info))]      <- 'r'
  names(info)[grepl('^t ', names(info))]      <- 'lag'
  names(media)[1] <- 'medium_id'
  fix <- info$medium_id %in% names(ZHANG_MEDIUM_FIX)
  info$medium_id[fix] <- ZHANG_MEDIUM_FIX[info$medium_id[fix]]

  if (!identical(names(curves)[-1], info$curve_id))
    stop(sprintf('Curves_%s / Info_%s curve IDs differ', assay, assay))
  miss <- setdiff(info$medium_id, media$medium_id)
  if (length(miss)) stop(sprintf('Assay %s: medium IDs without composition: %s',
                                 assay, paste(miss, collapse = ', ')))
  unk <- setdiff(info$abbr, ZHANG_SPECIES$abbr)
  if (length(unk)) stop('Unknown species abbreviation(s): ', paste(unk, collapse = ', '))
  list(curves = curves, info = info, media = media)
}

zhang_import_all <- function(
    con,
    source_dir  = NULL,
    parquet_dir = 'data/parquet/raw_measurements',
    overwrite   = FALSE,
    verbose     = TRUE
) {
  if (is.null(source_dir)) stop('source_dir is required: path to the Zhang 2026 Figshare files')
  requireNamespace('DBI'); requireNamespace('readxl')
  requireNamespace('data.table'); requireNamespace('arrow')
  .msg <- function(...) if (verbose) message('[zhang] ', ...)

  zhang_migrate_schema(con)
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)

  n_exist <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM dataset WHERE dataset_id LIKE 'zhang_2026_%'")$n
  if (n_exist > 0L && !overwrite) {
    .msg(n_exist, ' datasets already present — pass overwrite = TRUE to re-import')
    return(invisible(NULL))
  }

  all_ds <- list(); all_pl <- list(); all_wm <- list(); all_reg <- list()
  all_mc <- list(); all_met <- list()

  for (assay in names(ZHANG_ASSAYS)) {
    a   <- .zhang_read_assay(source_dir, assay)
    cfg <- ZHANG_ASSAYS[[assay]]
    .msg(sprintf('Assay %s: %d curves, %d media', assay, nrow(a$info),
                 length(unique(a$info$medium_id))))

    # Long format; drop time points not recorded (24 h runs are NA after 24 h)
    long <- data.table::melt(data.table::as.data.table(a$curves), id.vars = 'time_h',
                             variable.name = 'curve_id', value.name = 'value',
                             variable.factor = FALSE)
    long <- long[!is.na(long$value)]
    neg  <- long[long$value < 0, list(n_neg = .N), by = 'curve_id']

    # Composition: one row per medium × component, units from the column header
    comp_cols <- names(a$media)[-1]
    comp_name <- trimws(sub('\\s*\\(([^)]*)\\)\\s*$', '', comp_cols))
    comp_unit <- sub('.*\\(([^)]*)\\)\\s*$', '\\1', comp_cols)

    # Reference medium: repeated on every plate (one per tested medium) — assays II/III
    n_per_medium <- table(a$info$medium_id)
    ref_media <- if (assay %in% c('II', 'III')) names(n_per_medium)[n_per_medium > 100] else character(0)

    a$info$did <- paste0('zhang_2026_', tolower(a$info$abbr), '_', tolower(a$info$medium_id))
    for (did in unique(a$info$did)) {
      inf <- a$info[a$info$did == did, ]
      sp  <- ZHANG_SPECIES[ZHANG_SPECIES$abbr == inf$abbr[1], ]
      mid <- inf$medium_id[1]
      pid <- paste0(did, '__plate_1')
      wells    <- tolower(inf$curve_id)
      well_ids <- paste0(pid, '__', wells)
      is_ref   <- mid %in% ref_media

      d <- long[long$curve_id %in% inf$curve_id]
      d <- data.frame(dataset_id = did, plate_id = pid,
                      well_id = paste0(pid, '__', tolower(d$curve_id)),
                      well = tolower(d$curve_id), time_h = d$time_h,
                      signal_type = 'OD600', value = d$value, stringsAsFactors = FALSE)
      pq_path <- file.path(parquet_dir, paste0(did, '.parquet'))
      arrow::write_parquet(d, pq_path)

      neg_here <- neg[neg$curve_id %in% inf$curve_id]
      notes <- paste0(
        'Raw OD600 as deposited (not background-subtracted; authors subtract the t = 0 value ',
        'for their own analysis). Assay ', assay, ': ', cfg$desc, '. Medium ', mid, '. ',
        'Author curve IDs: ', paste(inf$curve_id, collapse = ', '), '. ',
        if ('well_location' %in% names(inf))
          paste0('Original well locations: ', paste(inf$well_location, collapse = ', '), '. ') else '',
        if (is_ref) sprintf('Reference medium repeated across plates (%d curves of this species). ', nrow(inf)) else '',
        if (nrow(neg_here)) paste0('Negative OD readings (plate-reader artefact, kept as recorded) in: ',
          paste(sprintf('%s (%d points)', neg_here$curve_id, neg_here$n_neg), collapse = ', '), '. ') else '',
        if (any(a$info$medium_id[a$info$did == did] == 'LE_c00009'))
          'Medium ID given as "LE_c0009" in Info_I.xlsx; corrected to LE_c00009. ' else '',
        if (assay == 'II') 'Temperature/instrument from Zhang & Ying 2025 Sci Rep (same strains and protocol).'
        else 'Temperature and instrument not reported in the data deposit.')

      all_ds[[did]] <- data.frame(
        dataset_id = did, raw_file = sprintf('Curves_%s.xlsx', assay),
        title = sprintf('%s in defined medium %s (Zhang 2026, assay %s)', sp$species, mid, assay),
        doi = ZHANG_DOI, year = ZHANG_YEAR, authors = ZHANG_AUTHORS, license = ZHANG_LICENSE,
        source_url = ZHANG_SOURCE_URL, time_unit = 'hours', notes = notes,
        stringsAsFactors = FALSE)
      all_pl[[did]] <- data.frame(
        plate_id = pid, dataset_id = did, plate_format = cfg$plate_format,
        plate_reader = cfg$plate_reader, wavelength_nm = 600, temperature_c = cfg$temperature_c,
        shake_freq_rpm = cfg$shake_rpm, stringsAsFactors = FALSE)
      all_wm[[did]] <- data.frame(
        well_id = well_ids, plate_id = pid, well = wells, species = sp$species,
        taxon_id = sp$taxon_id, strain = sp$strain, medium_state = 'liquid',
        medium_type = sprintf('Defined medium %s (assay %s, %s)', mid, assay, cfg$desc),
        perturbation_type = 'none', perturbation = NA_character_,
        concentration = NA_real_, concentration_unit = NA_character_,
        is_blank = FALSE, is_control = is_ref, replicate = seq_along(wells),
        dataset_id = did, gene_category = NA_character_, stringsAsFactors = FALSE)
      all_reg[[did]] <- data.frame(
        dataset_id = did, storage_type = 'parquet', path = pq_path, n_rows = nrow(d),
        n_wells = length(wells), signal_type = 'OD600', measurement_unit = 'OD',
        stringsAsFactors = FALSE)
      mrow <- a$media[a$media$medium_id == mid, comp_cols]
      all_mc[[did]] <- data.frame(
        dataset_id = did, compound = comp_name, conc_mm = NA_real_, conc_base_mm = NA_real_,
        is_stressor = FALSE, is_buffer_adj = FALSE,
        conc_value = as.numeric(unlist(mrow)), conc_unit = comp_unit, stringsAsFactors = FALSE)
      all_met[[did]] <- rbind(
        data.frame(dataset_id = did, well_id = well_ids, metric_name = 'carrying_capacity_od',
                   metric_value = inf$K, metric_unit = 'OD600'),
        data.frame(dataset_id = did, well_id = well_ids, metric_name = 'mumax_percapita',
                   metric_value = inf$r, metric_unit = '1/h'),
        data.frame(dataset_id = did, well_id = well_ids, metric_name = 'lag_time_h',
                   metric_value = inf$lag, metric_unit = 'h'))
    }
  }

  bind <- function(x) data.table::rbindlist(x, use.names = TRUE)
  met  <- bind(all_met)
  proc_id <- paste0('zhang_authors_', format(Sys.time(), '%Y%m%d_%H%M%S'))
  met <- data.frame(metric_id = paste0('zhang_m_', met$metric_name, '_', met$well_id),
                    processing_id = proc_id, met, model_name = 'authors')

  .msg(sprintf('Writing %d datasets, %d curves to DB...', length(all_ds), nrow(bind(all_wm))))
  DBI::dbWithTransaction(con, {
    if (n_exist > 0L) {
      for (sql in c(
        "DELETE FROM well_metadata WHERE plate_id LIKE 'zhang\\_2026\\_%' ESCAPE '\\'",
        "DELETE FROM derived_metric WHERE dataset_id LIKE 'zhang\\_2026\\_%' ESCAPE '\\'",
        "DELETE FROM processing_run WHERE processing_id LIKE 'zhang\\_authors\\_%' ESCAPE '\\'",
        "DELETE FROM medium_composition WHERE dataset_id LIKE 'zhang\\_2026\\_%' ESCAPE '\\'",
        "DELETE FROM raw_measurement_registry WHERE dataset_id LIKE 'zhang\\_2026\\_%' ESCAPE '\\'",
        "DELETE FROM dataset_conditions WHERE dataset_id LIKE 'zhang\\_2026\\_%' ESCAPE '\\'",
        "DELETE FROM plate WHERE dataset_id LIKE 'zhang\\_2026\\_%' ESCAPE '\\'",
        "DELETE FROM dataset WHERE dataset_id LIKE 'zhang\\_2026\\_%' ESCAPE '\\'"
      )) DBI::dbExecute(con, sql)
    }
    DBI::dbAppendTable(con, 'dataset', bind(all_ds))
    DBI::dbAppendTable(con, 'plate', bind(all_pl))
    DBI::dbAppendTable(con, 'well_metadata', bind(all_wm))
    DBI::dbAppendTable(con, 'raw_measurement_registry', bind(all_reg))
    DBI::dbAppendTable(con, 'medium_composition', bind(all_mc))
    DBI::dbExecute(con,
      "INSERT INTO processing_run
         (processing_id, dataset_id, tool_name, tool_version, correction_type, status, message)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(proc_id, 'zhang_all', 'authors', 'Zhang, Sawamura & Ying 2026',
                    'authors_t0_subtraction', 'completed',
                    'Author-computed K, r and lag time from Info_I/II/III.xlsx'))
    DBI::dbAppendTable(con, 'derived_metric', met)
  })
  qres_sync_conditions(con, names(all_ds))
  .msg(sprintf('Done: %d datasets, %d metrics', length(all_ds), nrow(met)))
  invisible(names(all_ds))
}
