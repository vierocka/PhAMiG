# R/import/aida_import.R
# ──────────────────────────────────────────────────────────────────────────────
# Batch import: Aida & Ying (2025) E. coli BW25113 chemically defined media
# Aida H & Ying BW (2025) Scientific Data 12:984
# DOI: 10.1038/s41597-025-05356-3  |  Figshare: 28342064
#
# Design: 1 condition (medium combination) = 1 dataset  (1,029 datasets total)
#   Each round tests 44 compounds × 4 concentration levels against a base medium.
#   KH2PO4 co-varies as a pH buffer adjuster — stored as is_buffer_adj = TRUE
#   in medium_composition; the primary stressor is the other changed compound.
#
# Pre-conditions:
#   source_dir must contain:
#     BW25113_Growth_Round01.xlsx … BW25113_Growth_Round07.xlsx
#     BW25113_Medium composition.xlsx
#     BW25113_GrowthDataEvaluation.xlsx
#   con            open DuckDB connection
#   qres_init_schema(con) already called (medium_composition table must exist)
#
# Usage:
#   source('R/import/aida_import.R')
#   aida_import_all(con,
#     source_dir = '/home/veve/Dropbox/GrowthDB_Qres/Ecoli_ChemicallyDefinedMedia_PopDyn')
# ──────────────────────────────────────────────────────────────────────────────

AIDA_DOI        <- '10.1038/s41597-025-05356-3'
AIDA_YEAR       <- 2025L
AIDA_AUTHORS    <- 'Aida H, Ying BW'
AIDA_TITLE_PFX  <- 'E. coli BW25113 CDM'
AIDA_LICENSE    <- 'CC BY-NC-ND 4.0'
AIDA_SOURCE_URL <- 'https://doi.org/10.6084/m9.figshare.28342064'
AIDA_BUFFER_CPD <- 'KH2PO4'  # always co-varies for pH; not the primary stressor

# ── Schema migration (idempotent) ─────────────────────────────────────────────
aida_migrate_schema <- function(con) {
  # well_metadata extra columns (shared with keio_migrate_schema — safe to repeat)
  for (sql in c(
    "ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS dataset_id TEXT",
    "ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS gene_category TEXT"
  )) tryCatch(DBI::dbExecute(con, sql), error = function(e) NULL)

  # medium_composition table — may not exist on DBs initialised before this table
  # was added to qres_init_schema
  tryCatch(DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS medium_composition (
      dataset_id    TEXT NOT NULL,
      compound      TEXT NOT NULL,
      conc_mm       DOUBLE,
      conc_base_mm  DOUBLE,
      is_stressor   BOOLEAN DEFAULT FALSE,
      is_buffer_adj BOOLEAN DEFAULT FALSE
    )"), error = function(e) NULL)

  invisible(TRUE)
}

# ── Identify base condition per round ─────────────────────────────────────────
# Returns named character vector: Round01 → 'Cond00007', etc.
# Base = condition with most biological replicates in that round.
.aida_base_ids <- function(mc_raw) {
  rep_counts <- table(mc_raw[['Condition_ID']])
  rounds     <- unique(mc_raw[['Round']])
  bases <- setNames(character(length(rounds)), rounds)
  for (rnd in rounds) {
    cids      <- unique(mc_raw[mc_raw[['Round']] == rnd, 'Condition_ID', drop = TRUE])
    bases[rnd] <- names(which.max(rep_counts[cids]))
  }
  bases
}

# ── Identify perturbation for one condition vs its round's base ───────────────
# Returns list: pert_type, pert, conc_mm, is_control, kh2po4_adjusts
.aida_perturbation <- function(cond_v, base_v, cpd_clean) {
  changed <- cpd_clean[abs(cond_v - base_v) > 1e-7]
  other   <- setdiff(changed, AIDA_BUFFER_CPD)

  if (length(changed) == 0L) {
    # This IS the base pattern
    list(pert_type = 'none', pert = NA_character_, conc_mm = NA_real_,
         is_control = TRUE, kh2po4_adjusts = FALSE)
  } else if (length(other) == 0L) {
    # KH2PO4 is the only thing varied — it IS the stressor here
    list(pert_type = 'chemical', pert = AIDA_BUFFER_CPD,
         conc_mm = unname(cond_v[AIDA_BUFFER_CPD]),
         is_control = FALSE, kh2po4_adjusts = FALSE)
  } else if (length(other) == 1L) {
    # Single stressor; KH2PO4 may co-vary as buffer adjuster
    list(pert_type = 'chemical', pert = other,
         conc_mm = unname(cond_v[other]),
         is_control = FALSE, kh2po4_adjusts = length(changed) > 1L)
  } else {
    # Multiple compounds change simultaneously (composite condition).
    # Abbreviate: first 3 names + count. Full composition is in medium_composition.
    n_other <- length(other)
    pert_lbl <- if (n_other <= 3L)
      paste(other, collapse = ' + ')
    else
      paste0(paste(other[1:3], collapse = ' + '), sprintf(' (+ %d more)', n_other - 3L))
    list(pert_type = 'chemical', pert = pert_lbl,
         conc_mm = NA_real_,
         is_control = FALSE, kh2po4_adjusts = AIDA_BUFFER_CPD %in% changed)
  }
}

# ── Main import ───────────────────────────────────────────────────────────────
aida_import_all <- function(
    con,
    source_dir  = '/home/veve/Dropbox/GrowthDB_Qres/Ecoli_ChemicallyDefinedMedia_PopDyn',
    parquet_dir = 'data/parquet/raw_measurements',
    overwrite   = FALSE,
    verbose     = TRUE
) {
  requireNamespace('DBI'); requireNamespace('readxl')
  requireNamespace('data.table'); requireNamespace('arrow')

  aida_migrate_schema(con)
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)
  .msg <- function(...) if (verbose) message('[aida] ', ...)

  # ── 1. Medium composition ──────────────────────────────────────────────────
  .msg('Loading medium composition...')
  mc_raw      <- as.data.frame(readxl::read_xlsx(
    file.path(source_dir, 'BW25113_Medium composition.xlsx'), sheet = 1))
  names(mc_raw)[1:3] <- c('Label', 'Assay_ID', 'Condition_ID')
  mc_raw[['Round']]  <- sub('_.*', '', mc_raw[['Assay_ID']])

  # Clean compound column names: strip "\r\n(mM)" suffix
  raw_cpd_cols <- names(mc_raw)[4:47]
  cpd_clean    <- gsub('[\r\n].*', '', raw_cpd_cols)   # 'KH2PO4\r\n(mM)' → 'KH2PO4'

  # One composition row per condition (use first appearance)
  cond_comp <- unique(mc_raw[, c('Condition_ID', 'Round', raw_cpd_cols)])

  # Label → Condition_ID + Round lookup
  label_map <- unique(mc_raw[, c('Label', 'Condition_ID', 'Round')])

  # ── 2. Base conditions and compound vectors ────────────────────────────────
  base_ids  <- .aida_base_ids(mc_raw)
  base_vecs <- lapply(base_ids, function(base_cid) {
    row <- cond_comp[cond_comp[['Condition_ID']] == base_cid, ][1L, raw_cpd_cols, drop = FALSE]
    setNames(as.numeric(row), cpd_clean)
  })

  # ── 3. Load all round Excel files once ────────────────────────────────────
  .msg('Loading round Excel files...')
  round_data <- list()
  for (rnd in paste0('Round0', 1:7)) {
    f <- file.path(source_dir, sprintf('BW25113_Growth_%s.xlsx', rnd))
    if (!file.exists(f)) { .msg('  Missing: ', f); next }
    d            <- as.data.frame(readxl::read_xlsx(f, sheet = 1))
    names(d)[1]  <- 'time_h'
    round_data[[rnd]] <- d
    .msg(sprintf('  %s: %d timepoints × %d curves', rnd, nrow(d), ncol(d) - 1L))
  }

  # ── 4. Import loop: one dataset per condition ──────────────────────────────
  all_conds  <- sort(unique(mc_raw[['Condition_ID']]))
  .msg(sprintf('%d conditions to import', length(all_conds)))

  ok <- 0L; skip <- 0L; err <- 0L

  for (i in seq_along(all_conds)) {
    cid <- all_conds[i]
    did <- paste0('aida_', tolower(cid))   # aida_cond00001

    if (i %% 200L == 0L)
      .msg(sprintf('%d / %d  ok=%d skip=%d err=%d', i, length(all_conds), ok, skip, err))

    # Skip if already present (unless overwrite)
    if (!overwrite) {
      n_exist <- tryCatch(
        DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM dataset WHERE dataset_id = ?",
                        params = list(did))$n, error = function(e) 0L)
      if (n_exist > 0L) { skip <- skip + 1L; next }
    }

    # All curve labels for this condition (may span multiple rounds)
    cond_labels <- label_map[label_map[['Condition_ID']] == cid, ]
    if (nrow(cond_labels) == 0L) { skip <- skip + 1L; next }

    # Primary round = first (alphabetical) round this condition appears in
    primary_rnd <- sort(unique(cond_labels[['Round']]))[1L]
    cond_v      <- setNames(
      as.numeric(cond_comp[cond_comp[['Condition_ID']] == cid &
                           cond_comp[['Round']] == primary_rnd, ][1L, raw_cpd_cols]),
      cpd_clean)
    base_v      <- base_vecs[[primary_rnd]]
    pert        <- .aida_perturbation(cond_v, base_v, cpd_clean)
    medium_type <- sprintf('CDM base %s (%s)', primary_rnd, base_ids[[primary_rnd]])

    # ── Extract and melt curves from all rounds ──────────────────────────────
    long_parts <- vector('list', length(round_data))
    for (rnd in unique(cond_labels[['Round']])) {
      rd <- round_data[[rnd]]
      if (is.null(rd)) next
      rnd_labels <- cond_labels[cond_labels[['Round']] == rnd, 'Label', drop = TRUE]
      avail      <- intersect(rnd_labels, names(rd))
      if (length(avail) == 0L) next
      sub <- rd[, c('time_h', avail), drop = FALSE]
      sub[, avail] <- lapply(sub[, avail, drop = FALSE], as.numeric)
      dt <- data.table::melt(
        data.table::as.data.table(sub),
        id.vars = 'time_h', variable.name = 'Label', value.name = 'value',
        variable.factor = FALSE)
      dt[['Label']] <- as.character(dt[['Label']])
      long_parts[[rnd]] <- dt
    }
    long_parts <- Filter(Negate(is.null), long_parts)
    if (length(long_parts) == 0L) { skip <- skip + 1L; next }
    long_dt <- data.table::rbindlist(long_parts)

    # well_id = plate_id + curve label (Labels are globally unique 1–13608)
    pid               <- paste0(did, '__plate_1')
    long_dt[['well_id']] <- paste0(pid, '__', tolower(long_dt[['Label']]))

    # ── Parquet ───────────────────────────────────────────────────────────────
    pq_path <- file.path(parquet_dir, paste0(did, '.parquet'))
    pq_ok   <- tryCatch({
      arrow::write_parquet(long_dt[, list(curve_id = well_id, time_h, value)], pq_path)
      TRUE
    }, error = function(e) {
      warning(sprintf('[aida] Parquet write failed %s: %s', did, e$message))
      FALSE
    })
    if (!pq_ok) { err <- err + 1L; next }

    # ── DB records ────────────────────────────────────────────────────────────
    tryCatch({
      for (tbl_sql in c(
        "DELETE FROM medium_composition WHERE dataset_id = ?",
        "DELETE FROM derived_metric     WHERE dataset_id = ?",
        "DELETE FROM processing_run     WHERE dataset_id = ?",
        "DELETE FROM raw_measurement_registry WHERE dataset_id = ?",
        "DELETE FROM plate              WHERE dataset_id = ?"
      )) DBI::dbExecute(con, tbl_sql, params = list(did))
      DBI::dbExecute(con, "DELETE FROM well_metadata WHERE plate_id LIKE ?",
                     params = list(paste0(pid, '%')))
      DBI::dbExecute(con, "DELETE FROM dataset WHERE dataset_id = ?", params = list(did))

      # Notes text
      rounds_str <- paste(sort(unique(cond_labels[['Round']])), collapse = ', ')
      buf_note <- if (pert$kh2po4_adjusts) {
        sprintf(' KH2PO4 adjusted for pH buffering (base: %.4g mM → %.4g mM).',
                unname(base_v[AIDA_BUFFER_CPD]), unname(cond_v[AIDA_BUFFER_CPD]))
      } else ''
      pert_note <- if (!is.na(pert$pert))
        sprintf(' Stressor: %s at %.4g mM.', pert$pert, pert$conc_mm)
      else ' Base pattern (no perturbation).'
      notes_txt <- paste0(
        'OD₆₀₀ medium-only wells pre-subtracted by authors (Aida & Ying 2025).',
        buf_note, pert_note,
        ' Round(s): ', rounds_str, '. Condition ID: ', cid, '.')

      # dataset
      title <- if (!is.na(pert$pert))
        sprintf('%s — %s at %.4g mM (%s)', AIDA_TITLE_PFX, pert$pert, pert$conc_mm, cid)
      else
        sprintf('%s — base pattern (%s)', AIDA_TITLE_PFX, cid)
      DBI::dbExecute(con,
        "INSERT INTO dataset
           (dataset_id, raw_file, title, doi, year, authors, license, source_url, time_unit, notes)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(did, paste0(cid, '.parquet'), title,
                      AIDA_DOI, AIDA_YEAR, AIDA_AUTHORS,
                      AIDA_LICENSE, AIDA_SOURCE_URL, 'hours', notes_txt))

      # plate
      DBI::dbExecute(con,
        "INSERT INTO plate
           (plate_id, dataset_id, plate_format, plate_reader, temperature_c, shake_freq_rpm, wavelength_nm)
         VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(pid, did, 96L, 'EPOCH2 (BioTek)', 37.0, 567.0, 600.0))

      # well_metadata — one row per curve/replicate
      well_ids <- unique(long_dt[['well_id']])
      n_wells  <- length(well_ids)
      wm_df <- data.frame(
        well_id           = well_ids,
        plate_id          = pid,
        dataset_id        = did,
        well              = sub('.*__', '', well_ids),      # 'curve00001'
        species           = 'E. coli',
        taxon_id          = '562',
        strain            = 'K-12 BW25113',
        medium_state      = 'liquid',
        medium_type       = medium_type,
        perturbation_type = pert$pert_type,
        perturbation      = pert$pert,
        concentration     = pert$conc_mm,
        concentration_unit= if (!is.na(pert$pert)) 'mM' else NA_character_,
        is_blank          = FALSE,
        is_control        = pert$is_control,
        replicate         = seq_len(n_wells),
        gene_category     = NA_character_,
        stringsAsFactors  = FALSE)
      DBI::dbWriteTable(con, 'well_metadata', wm_df, append = TRUE)

      # raw_measurement_registry
      DBI::dbExecute(con,
        "INSERT INTO raw_measurement_registry
           (dataset_id, storage_type, path, n_rows, n_wells, signal_type)
         VALUES (?, ?, ?, ?, ?, ?)",
        params = list(did, 'parquet', pq_path, nrow(long_dt), n_wells, 'OD600'))

      # medium_composition — all 44 compounds
      mc_df <- data.frame(
        dataset_id    = did,
        compound      = cpd_clean,
        conc_mm       = cond_v,
        conc_base_mm  = base_v,
        is_stressor   = cpd_clean %in% strsplit(pert$pert %||% '', ' \\+ ')[[1L]] &
                        !is.na(pert$pert),
        is_buffer_adj = cpd_clean == AIDA_BUFFER_CPD & pert$kh2po4_adjusts,
        stringsAsFactors = FALSE,
        row.names = NULL)
      DBI::dbWriteTable(con, 'medium_composition', mc_df, append = TRUE)

      ok <- ok + 1L
    }, error = function(e) {
      warning(sprintf('[aida] DB insert failed %s: %s', did, e$message))
      err <<- err + 1L
    })
  }

  tryCatch(qres_sync_conditions(con), error = function(e) NULL)
  .msg(sprintf('Import complete. Datasets: ok=%d  skip=%d  err=%d', ok, skip, err))

  # ── 5. Author K and r metrics ───────────────────────────────────────────────
  aida_import_metrics(con, source_dir, verbose = verbose)

  invisible(list(ok = ok, skip = skip, err = err))
}

# ── Metrics-only import (author-computed K and r) ─────────────────────────────
aida_import_metrics <- function(con, source_dir, overwrite = FALSE, verbose = TRUE) {
  .msg <- function(...) if (verbose) message('[aida] ', ...)

  if (!overwrite) {
    n_exist <- tryCatch(
      DBI::dbGetQuery(con,
        "SELECT COUNT(*) AS n FROM derived_metric WHERE metric_id LIKE 'aida_m_%'")$n,
      error = function(e) 0L)
    if (n_exist > 0L) {
      .msg(sprintf('Metrics already present (%d rows) — pass overwrite=TRUE to reimport', n_exist))
      return(invisible(NULL))
    }
  } else {
    DBI::dbExecute(con, "DELETE FROM derived_metric  WHERE metric_id   LIKE 'aida_m_%'")
    DBI::dbExecute(con, "DELETE FROM processing_run  WHERE processing_id LIKE 'aida_authors_%'")
  }

  .msg('Importing author K and r metrics...')

  ev <- suppressMessages(as.data.frame(readxl::read_xlsx(
    file.path(source_dir, 'BW25113_GrowthDataEvaluation.xlsx'), sheet = 1, skip = 1,
    na = c('', 'NA'))))
  names(ev)[1:7] <- c('Label', 'Assay_ID', 'Condition_ID', 'K', 'r', 'K_info', 'r_info')
  ev[['K']]      <- suppressWarnings(as.numeric(ev[['K']]))
  ev[['r']]      <- suppressWarnings(as.numeric(ev[['r']]))
  ev[['K_info']] <- suppressWarnings(as.integer(ev[['K_info']]))
  ev[['r_info']] <- suppressWarnings(as.integer(ev[['r_info']]))
  ev[['Round']]  <- sub('_.*', '', ev[['Assay_ID']])

  # Build well_id: same rule as import loop
  ev[['dataset_id']] <- paste0('aida_', tolower(ev[['Condition_ID']]))
  ev[['plate_id']]   <- paste0(ev[['dataset_id']], '__plate_1')
  ev[['well_id']]    <- paste0(ev[['plate_id']], '__', tolower(ev[['Label']]))

  # Only keep rows whose dataset was actually imported
  imported <- tryCatch(
    DBI::dbGetQuery(con,
      "SELECT dataset_id FROM dataset WHERE dataset_id LIKE 'aida_%'")$dataset_id,
    error = function(e) character(0))
  ev <- ev[ev[['dataset_id']] %in% imported, ]
  if (nrow(ev) == 0L) { .msg('No matching datasets found — run aida_import_all first'); return(invisible(NULL)) }

  # One processing_run record covering all Aida datasets
  proc_id <- paste0('aida_authors_', format(Sys.time(), '%Y%m%d_%H%M%S'))
  DBI::dbExecute(con,
    "INSERT INTO processing_run
       (processing_id, dataset_id, tool_name, tool_version, correction_type, status, message)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(proc_id, 'aida_all', 'authors_python_script',
                  'Aida & Ying 2025 (custom Python)',
                  'authors_subtraction', 'completed',
                  'Author-computed K and r from BW25113_GrowthDataEvaluation.xlsx'))

  # Build four metric types: K, r, K_info, r_info
  .make_metric <- function(name, values, unit) {
    data.frame(
      metric_id     = paste0('aida_m_', name, '_', ev[['well_id']]),
      processing_id = proc_id,
      dataset_id    = ev[['dataset_id']],
      well_id       = ev[['well_id']],
      metric_name   = name,
      metric_value  = as.numeric(values),
      metric_unit   = unit,
      model_name    = 'authors',
      stringsAsFactors = FALSE)
  }

  all_metrics <- rbind(
    .make_metric('carrying_capacity_od', ev[['K']],      'OD600'),
    .make_metric('mumax_percapita',       ev[['r']],      '1/h'),
    .make_metric('K_validation_flag',     ev[['K_info']], NA_character_),
    .make_metric('r_validation_flag',     ev[['r_info']], NA_character_))

  DBI::dbWriteTable(con, 'derived_metric', all_metrics, append = TRUE)
  .msg(sprintf('Metrics: %d rows (%d curves)', nrow(all_metrics), nrow(ev)))
  invisible(all_metrics)
}

# ── UniProt link for a compound (chemical search) ─────────────────────────────
aida_uniprot_url <- function(compound) {
  safe <- utils::URLencode(compound, reserved = TRUE)
  paste0('https://www.uniprot.org/uniprotkb?query=', safe,
         '+AND+taxonomy_id%3A83333+AND+reviewed%3Atrue')
}
