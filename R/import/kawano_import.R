# R/import/kawano_import.R
# ──────────────────────────────────────────────────────────────────────────────
# Batch import: Kawano T, Araki S, Ying BW (2026)
# "Bacterial growth dynamics under medium-drug combinations"
# Figshare: 10.6084/m9.figshare.33716665 (CC BY 4.0, published 2026-10-01)
# No associated article yet — temperature / instrument are not documented.
#
# 11,148 raw OD600 curves of E. coli BW25113 (30 min interval, 0.5–48 h) under
# 1,490 conditions = one of 8 antibiotics (or a mix, or none) at a fixed dose in
# the Aida 2025 chemically defined base medium with 0–5 of its 44 compounds changed.
#
# Units: all Media.xlsx values are mM. The file has no unit labels, but the 44
# compounds are the Aida & Ying (2025) set with identical base values to Aida's
# mM-labelled file, and drug values convert to round µg/ml (e.g. chloramphenicol
# 0.0154736 mM × 323.13 g/mol = 5.000 µg/ml). Drug concentrations refer to the
# salt form named in the column (e.g. KanamycinSulfate).
#
# Design: 1 condition = 1 dataset (1,490), as in aida_import.R.
#   perturbation = drug(s); medium_type = base CDM or the compounds changed vs Cond0001
#   (the no-drug base medium). Full composition → medium_composition (mM).
# Author metrics (Parameters.xlsx): K, r (1/h), t (h, lag), K_info, r_info flags.
#
# Usage:
#   source('R/import/kawano_import.R')
#   kawano_import_all(con, source_dir = '/path/to/Kawano_2026_Ecoli_medium_drug')
# ──────────────────────────────────────────────────────────────────────────────

KAWANO_SOURCE_DOI <- '10.6084/m9.figshare.33716665'
KAWANO_YEAR       <- 2026L
KAWANO_AUTHORS    <- 'Kawano T, Araki S, Ying BW'
KAWANO_LICENSE    <- 'CC BY 4.0'
KAWANO_SOURCE_URL <- 'https://doi.org/10.6084/m9.figshare.33716665'
KAWANO_BASE_ID    <- 'Cond0001'   # Drug_ID NA_001: no drug, base medium

# Media.xlsx drug column → searchable drug name
KAWANO_DRUGS <- c(AmpicillinSodium = 'Ampicillin', GentamicinSulfate = 'Gentamicin',
                  CiprofloxacinHydrochlorideMonohydrate = 'Ciprofloxacin',
                  Chloramphenicol = 'Chloramphenicol', Rifampicin = 'Rifampicin',
                  Erythromycin = 'Erythromycin', NalidixicAcid = 'Nalidixic acid',
                  KanamycinSulfate = 'Kanamycin')

kawano_import_all <- function(
    con,
    source_dir  = NULL,
    parquet_dir = 'data/parquet/raw_measurements',
    overwrite   = FALSE,
    verbose     = TRUE
) {
  if (is.null(source_dir)) stop('source_dir is required: path to the Figshare 33716665 files')
  requireNamespace('DBI'); requireNamespace('readxl')
  requireNamespace('data.table'); requireNamespace('arrow')
  .msg <- function(...) if (verbose) message('[kawano] ', ...)
  for (sql in c("ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS dataset_id TEXT",
                "ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS gene_category TEXT",
                "ALTER TABLE medium_composition ADD COLUMN IF NOT EXISTS conc_value DOUBLE",
                "ALTER TABLE medium_composition ADD COLUMN IF NOT EXISTS conc_unit TEXT"))
    tryCatch(DBI::dbExecute(con, sql), error = function(e) NULL)
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)

  n_exist <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM dataset WHERE dataset_id LIKE 'kawano_2026_%'")$n
  if (n_exist > 0L && !overwrite) {
    .msg(n_exist, ' datasets already present — pass overwrite = TRUE to re-import')
    return(invisible(NULL))
  }

  rd <- function(f) suppressMessages(as.data.frame(readxl::read_xlsx(file.path(source_dir, f))))
  media <- rd('Media.xlsx'); pars <- rd('Parameters.xlsx')
  names(pars) <- c('curve_id', 'cond_id', 'well_location', 'K', 'r', 'lag', 'K_info', 'r_info')
  curves <- lapply(c('Curves_Part_I.xlsx', 'Curves_Part_II.xlsx'), function(f) {
    d <- rd(f); names(d)[1] <- 'time_h'
    data.table::melt(data.table::as.data.table(d), id.vars = 'time_h', variable.name = 'curve_id',
                     value.name = 'value', variable.factor = FALSE)
  })
  long <- data.table::rbindlist(curves)
  long <- long[!is.na(long$value)]
  if (!setequal(unique(long$curve_id), pars$curve_id))
    stop('Curve IDs in Curves_Part_I/II and Parameters.xlsx differ')
  miss <- setdiff(pars$cond_id, media$Condition_ID)
  if (length(miss)) stop('Conditions without medium composition: ', paste(miss, collapse = ', '))
  .msg(sprintf('%d curves, %d conditions, %d data points',
               nrow(pars), length(unique(pars$cond_id)), nrow(long)))

  drug_cols <- names(KAWANO_DRUGS)
  cmp_cols  <- setdiff(names(media), c('Condition_ID', 'Drug_ID', drug_cols))
  if (length(cmp_cols) != 44L || !all(drug_cols %in% names(media)))
    stop('Unexpected Media.xlsx columns')
  base <- unlist(media[media$Condition_ID == KAWANO_BASE_ID, cmp_cols])
  neg  <- long[long$value < 0, list(n_neg = .N), by = 'curve_id']

  all_ds <- list(); all_pl <- list(); all_wm <- list(); all_reg <- list()
  all_mc <- list(); all_met <- list()

  for (cid in sort(unique(pars$cond_id))) {
    p   <- pars[pars$cond_id == cid, ]
    m   <- media[media$Condition_ID == cid, ]
    did <- paste0('kawano_2026_', tolower(cid))
    pid <- paste0(did, '__plate_1')
    wells    <- tolower(p$curve_id)
    well_ids <- paste0(pid, '__', wells)

    cv      <- unlist(m[, cmp_cols])
    changed <- cmp_cols[abs(cv - base) > 1e-9]
    dv      <- unlist(m[, drug_cols]); on <- drug_cols[dv > 0]
    drug_lbl <- if (length(on)) paste(KAWANO_DRUGS[on], collapse = ' + ') else NA_character_
    fmt <- function(x) formatC(x, digits = 4, format = 'g')
    med_lbl <- if (length(changed)) paste0('CDM modified: ', paste(
      sprintf('%s %s mM (base %s)', changed, fmt(cv[changed]), fmt(base[changed])), collapse = '; '))
      else 'CDM base (Aida 2025 base medium)'
    is_base <- cid == KAWANO_BASE_ID

    d <- long[long$curve_id %in% p$curve_id]
    pq <- data.frame(dataset_id = did, plate_id = pid, well_id = paste0(pid, '__', tolower(d$curve_id)),
                     well = tolower(d$curve_id), time_h = d$time_h, signal_type = 'OD600',
                     value = d$value, stringsAsFactors = FALSE)
    pq_path <- file.path(parquet_dir, paste0(did, '.parquet'))
    arrow::write_parquet(pq, pq_path)

    neg_here <- neg[neg$curve_id %in% p$curve_id]
    all_ds[[did]] <- data.frame(
      dataset_id = did, raw_file = 'Curves_Part_I.xlsx; Curves_Part_II.xlsx',
      title = sprintf('E. coli BW25113 — %s, %s (Kawano 2026, %s)',
                      if (is.na(drug_lbl)) 'no drug' else drug_lbl,
                      if (length(changed)) paste(changed, collapse = ', ') else 'base CDM', cid),
      doi = KAWANO_SOURCE_DOI, year = KAWANO_YEAR, authors = KAWANO_AUTHORS,
      license = KAWANO_LICENSE, source_url = KAWANO_SOURCE_URL, time_unit = 'hours',
      notes = paste0(
        'Raw OD600 as deposited (not background-subtracted); first time point 0.5 h. ',
        'Condition ', cid, ', Drug_ID ', m$Drug_ID, '. ',
        if (length(on)) paste0('Drug(s): ', paste(sprintf('%s %s mM', on, fmt(dv[on])), collapse = ', '), '. ') else 'No drug. ',
        'Medium: ', med_lbl, '. Author curve IDs: ', paste(p$curve_id, collapse = ', '),
        '. Original well locations: ', paste(p$well_location, collapse = ', '), '. ',
        if (nrow(neg_here)) paste0('Negative OD readings (kept as recorded) in: ',
          paste(sprintf('%s (%d points)', neg_here$curve_id, neg_here$n_neg), collapse = ', '), '. ') else '',
        'Data deposit only (no article yet): temperature and instrument not documented.'),
      stringsAsFactors = FALSE)
    all_pl[[did]] <- data.frame(
      plate_id = pid, dataset_id = did, plate_format = '96', plate_reader = NA_character_,
      wavelength_nm = 600, temperature_c = NA_real_, shake_freq_rpm = NA_real_,
      stringsAsFactors = FALSE)
    all_wm[[did]] <- data.frame(
      well_id = well_ids, plate_id = pid, well = wells, species = 'E. coli', taxon_id = '562',
      strain = 'K-12 BW25113', medium_state = 'liquid', medium_type = med_lbl,
      perturbation_type = if (is_base) 'none' else 'chemical',
      perturbation = if (!is.na(drug_lbl)) drug_lbl
                     else if (length(changed)) paste(changed, collapse = ' + ') else NA_character_,
      concentration = if (length(on) == 1L) unname(dv[on])
                      else if (is.na(drug_lbl) && length(changed) == 1L) unname(cv[changed]) else NA_real_,
      concentration_unit = if (length(on) == 1L || (is.na(drug_lbl) && length(changed) == 1L)) 'mM' else NA_character_,
      is_blank = FALSE, is_control = is_base, replicate = seq_along(wells),
      dataset_id = did, gene_category = NA_character_, stringsAsFactors = FALSE)
    all_reg[[did]] <- data.frame(
      dataset_id = did, storage_type = 'parquet', path = pq_path, n_rows = nrow(pq),
      n_wells = length(wells), signal_type = 'OD600', measurement_unit = 'OD',
      stringsAsFactors = FALSE)
    all_mc[[did]] <- data.frame(
      dataset_id = did, compound = c(cmp_cols, drug_cols),
      conc_mm = c(cv, dv), conc_base_mm = c(base, rep(0, length(drug_cols))),
      is_stressor = c(cmp_cols %in% changed, dv > 0), is_buffer_adj = FALSE,
      conc_value = c(cv, dv), conc_unit = 'mM', stringsAsFactors = FALSE, row.names = NULL)
    mk <- function(name, v, unit) data.frame(dataset_id = did, well_id = well_ids, metric_name = name,
                                             metric_value = as.numeric(v), metric_unit = unit)
    all_met[[did]] <- rbind(mk('carrying_capacity_od', p$K, 'OD600'), mk('mumax_percapita', p$r, '1/h'),
                            mk('lag_time_h', p$lag, 'h'), mk('K_validation_flag', p$K_info, NA_character_),
                            mk('r_validation_flag', p$r_info, NA_character_))
  }

  bind <- function(x) data.table::rbindlist(x, use.names = TRUE)
  met <- bind(all_met)
  proc_id <- paste0('kawano_authors_', format(Sys.time(), '%Y%m%d_%H%M%S'))
  met <- data.frame(metric_id = paste0('kawano_m_', met$metric_name, '_', met$well_id),
                    processing_id = proc_id, met, model_name = 'authors')

  .msg(sprintf('Writing %d datasets to DB...', length(all_ds)))
  DBI::dbWithTransaction(con, {
    if (n_exist > 0L) for (sql in c(
      "DELETE FROM well_metadata WHERE plate_id LIKE 'kawano\\_2026\\_%' ESCAPE '\\'",
      "DELETE FROM derived_metric WHERE dataset_id LIKE 'kawano\\_2026\\_%' ESCAPE '\\'",
      "DELETE FROM processing_run WHERE processing_id LIKE 'kawano\\_authors\\_%' ESCAPE '\\'",
      "DELETE FROM medium_composition WHERE dataset_id LIKE 'kawano\\_2026\\_%' ESCAPE '\\'",
      "DELETE FROM raw_measurement_registry WHERE dataset_id LIKE 'kawano\\_2026\\_%' ESCAPE '\\'",
      "DELETE FROM dataset_conditions WHERE dataset_id LIKE 'kawano\\_2026\\_%' ESCAPE '\\'",
      "DELETE FROM plate WHERE dataset_id LIKE 'kawano\\_2026\\_%' ESCAPE '\\'",
      "DELETE FROM dataset WHERE dataset_id LIKE 'kawano\\_2026\\_%' ESCAPE '\\'"
    )) DBI::dbExecute(con, sql)
    DBI::dbAppendTable(con, 'dataset', bind(all_ds))
    DBI::dbAppendTable(con, 'plate', bind(all_pl))
    DBI::dbAppendTable(con, 'well_metadata', bind(all_wm))
    DBI::dbAppendTable(con, 'raw_measurement_registry', bind(all_reg))
    DBI::dbAppendTable(con, 'medium_composition', bind(all_mc))
    DBI::dbExecute(con,
      "INSERT INTO processing_run
         (processing_id, dataset_id, tool_name, tool_version, correction_type, status, message)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(proc_id, 'kawano_all', 'authors', 'Kawano, Araki & Ying 2026',
                    'authors', 'completed', 'Author K, r, lag and K/r validation flags from Parameters.xlsx'))
    DBI::dbAppendTable(con, 'derived_metric', met)
  })
  qres_sync_conditions(con, names(all_ds))
  .msg(sprintf('Done: %d datasets, %d metrics', length(all_ds), nrow(met)))
  invisible(names(all_ds))
}
