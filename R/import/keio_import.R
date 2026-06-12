# R/import/keio_import.R
# ──────────────────────────────────────────────────────────────────────────────
# Batch import: Keio E. coli single-gene knockout growth curves
# Lao Z & Ying BW (2026) Scientific Data 13:717
# DOI: 10.1038/s41597-026-07075-9  |  Figshare: 28342043
#
# Dataset granularity: 1 strain (JWID) × 1 medium = 1 dataset, 3 replicate wells
# Total: 3,909 genes × 2 media = 7,818 datasets
#
# Pre-conditions:
#   source_dir  must contain Growth_curves_LB.csv, Growth_curves_M63.csv,
#               Curves_knockouts_media.xlsx, Growth_parameters.xlsx,
#               Bacterial stocks.xlsx
#   con         open DuckDB connection (qres_connect)
#   qres_init_schema(con) already called
#
# Usage:
#   source('R/import/keio_import.R')
#   keio_import_all(con,
#     source_dir = '/path/to/Ecoli_3909_singleGeneKO/28342043')
# ──────────────────────────────────────────────────────────────────────────────

# Full names for the 22 E. coli K-12 gene functional categories (Table 2, paper)
KEIO_CAT_LABELS <- c(
  e   = 'Enzyme',
  o   = 'Unknown function',
  pe  = 'Enzyme, predicted',
  t   = 'Transporter',
  pt  = 'Transporter, predicted',
  r   = 'Regulator',
  h   = 'Phage/IS in common',
  pm  = 'Membrane, predicted',
  pr  = 'Regulator, predicted',
  d   = 'Partial information',
  f   = 'Factor',
  c   = 'Carrier',
  su  = 'Pseudogenes in common',
  pf  = 'Factor, predicted',
  s   = 'Structural component',
  lp  = 'Lipoprotein',
  m   = 'Membrane',
  pc  = 'Carrier, predicted',
  cp  = 'Cell process',
  ps  = 'Structural component, predicted',
  l   = 'Leader peptide',
  ph  = 'Phage/IS in common, predicted'
)

# Apply schema additions needed for Keio-specific fields.
# Safe to call multiple times (IF NOT EXISTS / tryCatch).
keio_migrate_schema <- function(con) {
  for (sql in c(
    # dataset_id in well_metadata — used by qres_get_blank_wells and other queries
    "ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS dataset_id TEXT",
    # gene functional category abbreviation (e, o, pe, t …)
    "ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS gene_category TEXT"
  )) tryCatch(DBI::dbExecute(con, sql), error = function(e) NULL)
  invisible(TRUE)
}

# ── Main import function ──────────────────────────────────────────────────────
keio_import_all <- function(
    con,
    source_dir  = '/home/veve/Dropbox/GrowthDB_Qres/Ecoli_3909_singleGeneKO/28342043',
    parquet_dir = 'data/parquet/raw_measurements',
    overwrite   = FALSE,   # TRUE: re-import datasets that already exist in DB
    verbose     = TRUE
) {
  requireNamespace('DBI')
  requireNamespace('readxl')
  requireNamespace('data.table')

  keio_migrate_schema(con)
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)

  # ── 1. Load source files ──────────────────────────────────────────────────
  .msg <- function(...) if (verbose) message('[keio] ', ...)

  .msg('Reading metadata files...')
  meta <- as.data.frame(readxl::read_excel(
    file.path(source_dir, 'Curves_knockouts_media.xlsx')))
  names(meta)[1] <- 'curve_id'
  names(meta)    <- c('curve_id', 'jwid', 'gene_name', 'gene_cat', 'medium')

  gp <- as.data.frame(readxl::read_excel(
    file.path(source_dir, 'Growth_parameters.xlsx')))
  names(gp) <- c('curve_id', 'medium', 'K', 'time_K', 'indicator_K',
                  'r', 'time_r', 'indicator_r')

  alt_stocks <- as.data.frame(readxl::read_excel(
    file.path(source_dir, 'Bacterial stocks.xlsx')))
  alt_jwids  <- alt_stocks$JWID   # 46 strains prepared from liquid culture

  .msg('Reading LB growth curves CSV (190 × 11728)...')
  lb_wide  <- as.data.frame(data.table::fread(
    file.path(source_dir, 'Growth_curves_LB.csv'), data.table = FALSE))
  names(lb_wide)[1] <- 'time_h'   # original: "Time（h）" with full-width parens

  .msg('Reading M63 growth curves CSV (190 × 11728)...')
  m63_wide <- as.data.frame(data.table::fread(
    file.path(source_dir, 'Growth_curves_M63.csv'), data.table = FALSE))
  names(m63_wide)[1] <- 'time_h'

  # ── 2. Build dataset list (one row per gene × medium) ────────────────────
  strain_tbl <- unique(meta[, c('jwid', 'gene_name', 'gene_cat')])
  datasets   <- rbind(
    cbind(strain_tbl, medium = 'LB',  stringsAsFactors = FALSE),
    cbind(strain_tbl, medium = 'M63', stringsAsFactors = FALSE)
  )
  # dataset_id: keio_jw1950_lb  (lowercase, consistent key)
  datasets$dataset_id <- paste0('keio_', tolower(datasets$jwid), '_',
                                 tolower(datasets$medium))

  n_total <- nrow(datasets)
  n_ok <- 0L; n_skip <- 0L; n_err <- 0L

  .msg(sprintf('Starting import of %d datasets (overwrite=%s)...', n_total, overwrite))

  # ── 3. Loop: one dataset per gene × medium ────────────────────────────────
  for (i in seq_len(n_total)) {
    ds  <- datasets[i, , drop = FALSE]
    did <- ds$dataset_id

    # Skip if already in DB and overwrite = FALSE
    if (!overwrite) {
      exists <- DBI::dbGetQuery(con,
        'SELECT 1 FROM dataset WHERE dataset_id = ?', params = list(did))
      if (nrow(exists) > 0) { n_skip <- n_skip + 1L; next }
    }

    # ── 3a. Find the 3 replicate curve IDs ─────────────────────────────
    rep_rows <- meta[meta$jwid == ds$jwid & meta$medium == ds$medium, ]
    if (nrow(rep_rows) != 3L) {
      warning(sprintf('[keio] Expected 3 replicates for %s, got %d — skipping',
                      did, nrow(rep_rows)))
      n_err <- n_err + 1L; next
    }
    cids <- rep_rows$curve_id   # e.g. c("Curve11728","Curve11729","Curve11730")

    # ── 3b. Extract time series from wide CSV ───────────────────────────
    wide_src <- if (ds$medium == 'LB') lb_wide else m63_wide
    missing  <- setdiff(cids, names(wide_src))
    if (length(missing) > 0) {
      warning(sprintf('[keio] Columns not found in %s CSV: %s — skipping %s',
                      ds$medium, paste(missing, collapse = ','), did))
      n_err <- n_err + 1L; next
    }

    sub_wide        <- wide_src[, c('time_h', cids), drop = FALSE]
    names(sub_wide) <- c('time_h', 'R1', 'R2', 'R3')
    sub_wide[, 2:4] <- lapply(sub_wide[, 2:4], as.numeric)   # ensure uniform double type

    # ── 3c. Long format ─────────────────────────────────────────────────
    plate_id <- paste0(did, '__plate_1')
    long <- data.table::melt(
      data.table::as.data.table(sub_wide),
      id.vars         = 'time_h',
      variable.name   = 'well',
      value.name      = 'value',
      variable.factor = FALSE
    )
    long[, dataset_id  := did]
    long[, plate_id    := plate_id]
    long[, well_id     := paste0(plate_id, '__', well)]
    long[, signal_type := 'OD600']
    data.table::setcolorder(
      long, c('dataset_id', 'plate_id', 'well_id', 'well', 'time_h', 'signal_type', 'value'))

    # ── 3d. Write Parquet ────────────────────────────────────────────────
    pq_path <- file.path(parquet_dir, paste0(did, '.parquet'))
    pq_ok   <- tryCatch({
      if (requireNamespace('arrow', quietly = TRUE)) {
        arrow::write_parquet(as.data.frame(long), pq_path)
        'parquet'
      } else {
        csv_path <- sub('\\.parquet$', '_long.csv', pq_path)
        data.table::fwrite(long, csv_path)
        pq_path <- csv_path
        'csv_long'
      }
    }, error = function(e) {
      warning(sprintf('[keio] Write failed for %s: %s', did, e$message))
      NULL
    })
    if (is.null(pq_ok)) { n_err <- n_err + 1L; next }

    # ── 3e. Metadata strings ─────────────────────────────────────────────
    cat_abbr  <- ds$gene_cat
    cat_full  <- KEIO_CAT_LABELS[cat_abbr] %||% cat_abbr
    inoculum  <- if (ds$jwid %in% alt_jwids)
      'liquid (alternative stock — cultured in test tube before freezing)'
      else 'solid (agar plate standard stock)'
    medium_long <- if (ds$medium == 'LB') 'LB (Luria-Bertani)' else 'M63 minimal medium'

    notes_txt <- paste0(
      'Keio collection — Lao Z & Ying BW (2026) Scientific Data 13:717. ',
      'Gene: ', ds$gene_name, ' (', ds$jwid, '). ',
      'Functional category: ', cat_full, ' (', cat_abbr, '). ',
      'Inoculum source: ', inoculum, '. ',
      'IMPORTANT: OD₆₀₀ plate-mean at t=0 pre-subtracted by authors — ',
      'set baseline correction to none in analysis.'
    )

    # ── 3f. DB inserts (delete-then-insert for idempotency) ──────────────
    ok <- tryCatch({
      DBI::dbExecute(con, 'DELETE FROM raw_measurement_registry WHERE dataset_id = ?',
                     params = list(did))
      DBI::dbExecute(con, 'DELETE FROM well_metadata WHERE plate_id = ?',
                     params = list(plate_id))
      DBI::dbExecute(con, 'DELETE FROM plate WHERE dataset_id = ?',    params = list(did))
      DBI::dbExecute(con, 'DELETE FROM dataset WHERE dataset_id = ?',  params = list(did))

      # dataset
      DBI::dbExecute(con,
        "INSERT INTO dataset
           (dataset_id, raw_file, title, doi, year, authors,
            license, source_url, time_unit, notes)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          did,
          paste0(ds$jwid, '_', ds$medium, '.parquet'),
          paste0('Keio Δ', ds$gene_name, ' (', ds$jwid, ') — ', ds$medium),
          '10.1038/s41597-026-07075-9',
          2026L,
          'Lao Z, Ying BW',
          'CC BY-NC-ND 4.0',
          'https://doi.org/10.6084/m9.figshare.28342043',
          'hours',
          notes_txt
        )
      )

      # plate — EPOCH2 BioTek, 37 °C, 567 rpm, 96-well flat-bottom clear
      DBI::dbExecute(con,
        "INSERT INTO plate
           (plate_id, dataset_id, plate_format, plate_reader, temperature_c, shake_freq_rpm)
         VALUES (?, ?, ?, ?, ?, ?)",
        params = list(plate_id, did, '96', 'EPOCH2 (BioTek)', 37.0, 567.0)
      )

      # well_metadata — R1, R2, R3
      for (rep_n in 1:3) {
        wlbl    <- paste0('R', rep_n)
        well_id <- paste0(plate_id, '__', wlbl)
        DBI::dbExecute(con,
          "INSERT INTO well_metadata
             (well_id, plate_id, dataset_id, well, species, taxon_id, strain,
              medium_state, medium_type, perturbation_type, perturbation,
              is_blank, is_control, replicate, gene_category)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
          params = list(
            well_id, plate_id, did, wlbl,
            'E. coli', '562',
            paste0('K-12 BW25113 Δ', ds$gene_name),
            'liquid',
            medium_long,
            'genetic',
            ds$gene_name,
            FALSE, FALSE,
            rep_n,
            cat_abbr
          )
        )
      }

      # raw_measurement_registry
      DBI::dbExecute(con,
        "INSERT INTO raw_measurement_registry
           (dataset_id, storage_type, path, n_rows, n_wells, signal_type)
         VALUES (?, ?, ?, ?, ?, ?)",
        params = list(did, pq_ok, pq_path, nrow(long), 3L, 'OD600')
      )

      TRUE
    }, error = function(e) {
      warning(sprintf('[keio] DB insert failed for %s: %s', did, e$message))
      FALSE
    })

    if (isTRUE(ok)) n_ok <- n_ok + 1L else n_err <- n_err + 1L

    if (verbose && i %% 500 == 0)
      message(sprintf('[keio] %d / %d  ok=%d  skip=%d  err=%d',
                      i, n_total, n_ok, n_skip, n_err))
  }

  # ── 4. Import author-computed growth parameters (K and r) ────────────────
  .msg('Importing K and r from Growth_parameters.xlsx...')

  proc_id <- paste0('keio_authors_', format(Sys.time(), '%Y%m%d_%H%M%S'))
  DBI::dbExecute(con,
    "INSERT INTO processing_run
       (processing_id, tool_name, tool_version, correction_type, status, message)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = list(
      proc_id,
      'authors_python_script',
      'Lao & Ying 2026 (custom Python)',
      'plate_mean_t0',
      'completed',
      paste0('K (carrying capacity) and r (max growth rate) from Growth_parameters.xlsx. ',
             'Computed by Lao & Ying using their custom Python program. ',
             'indicator_K: 0=valid, 1=biological bias (incomplete growth), 3=replicate outlier. ',
             'indicator_r: 0=valid, 2=computational bias (noisy slope), 3=replicate outlier.')
    )
  )

  # Join growth parameters with curve-level metadata to get JWID.
  # medium comes from gp (not meta) to avoid medium.x / medium.y column collision.
  gp_full <- merge(gp, meta[, c('curve_id', 'jwid')], by = 'curve_id')
  gp_full$dataset_id <- paste0('keio_', tolower(gp_full$jwid), '_', tolower(gp_full$medium))
  gp_full <- gp_full[order(gp_full$dataset_id, gp_full$curve_id), ]
  gp_full$rep_n   <- ave(seq_len(nrow(gp_full)), gp_full$dataset_id,
                          FUN = function(x) seq_along(x))
  gp_full$well_id <- paste0(gp_full$dataset_id, '__plate_1__R', gp_full$rep_n)

  # Only insert metrics for datasets that were actually imported this run
  imported_ids <- DBI::dbGetQuery(con,
    "SELECT dataset_id FROM dataset WHERE dataset_id LIKE 'keio_%'")$dataset_id
  gp_full <- gp_full[gp_full$dataset_id %in% imported_ids, ]

  if (nrow(gp_full) > 0) {
    metrics <- rbind(
      data.frame(processing_id = proc_id, dataset_id = gp_full$dataset_id,
                 well_id = gp_full$well_id, metric_name = 'K',
                 metric_value = as.numeric(gp_full$K),
                 metric_unit = 'OD600', model_name = 'authors',
                 stringsAsFactors = FALSE),
      data.frame(processing_id = proc_id, dataset_id = gp_full$dataset_id,
                 well_id = gp_full$well_id, metric_name = 'r',
                 metric_value = as.numeric(gp_full$r),
                 metric_unit = '1/h', model_name = 'authors',
                 stringsAsFactors = FALSE),
      data.frame(processing_id = proc_id, dataset_id = gp_full$dataset_id,
                 well_id = gp_full$well_id, metric_name = 'indicator_K',
                 metric_value = as.numeric(gp_full$indicator_K),
                 metric_unit = NA_character_, model_name = 'authors',
                 stringsAsFactors = FALSE),
      data.frame(processing_id = proc_id, dataset_id = gp_full$dataset_id,
                 well_id = gp_full$well_id, metric_name = 'indicator_r',
                 metric_value = as.numeric(gp_full$indicator_r),
                 metric_unit = NA_character_, model_name = 'authors',
                 stringsAsFactors = FALSE)
    )
    metrics$metric_id <- paste0('keio_m_', seq_len(nrow(metrics)))
    DBI::dbWriteTable(con, 'derived_metric', metrics, append = TRUE)
    .msg(sprintf('Inserted %d metric rows.', nrow(metrics)))
  }

  .msg(sprintf(
    'Import complete. Datasets: ok=%d  skip=%d  err=%d  |  Metrics: %d rows.',
    n_ok, n_skip, n_err, if (exists('metrics')) nrow(metrics) else 0L))

  invisible(list(n_ok = n_ok, n_skip = n_skip, n_err = n_err))
}

# ── Metrics-only import (run after datasets are already in DB) ───────────────
# Use this when keio_import_all() completed with "Metrics: 0 rows" due to the
# medium-column merge bug, or to re-import metrics without re-importing raw data.
keio_import_metrics <- function(
    con,
    source_dir = '/home/veve/Dropbox/GrowthDB_Qres/Ecoli_3909_singleGeneKO/28342043',
    verbose    = TRUE
) {
  requireNamespace('DBI'); requireNamespace('readxl')

  .msg <- function(...) if (verbose) message('[keio-metrics] ', ...)

  # Guard: skip if metrics already exist
  existing <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM derived_metric WHERE metric_id LIKE 'keio_m_%'")$n
  if (existing > 0L) {
    .msg(sprintf('Metrics already present (%d rows). Use overwrite=TRUE to replace.', existing))
    return(invisible(NULL))
  }

  .msg('Reading source files...')
  meta <- as.data.frame(readxl::read_excel(
    file.path(source_dir, 'Curves_knockouts_media.xlsx')))
  names(meta)[1] <- 'curve_id'
  names(meta)    <- c('curve_id', 'jwid', 'gene_name', 'gene_cat', 'medium')

  gp <- as.data.frame(readxl::read_excel(
    file.path(source_dir, 'Growth_parameters.xlsx')))
  names(gp) <- c('curve_id', 'medium', 'K', 'time_K', 'indicator_K',
                  'r', 'time_r', 'indicator_r')

  proc_id <- paste0('keio_authors_', format(Sys.time(), '%Y%m%d_%H%M%S'))
  DBI::dbExecute(con,
    "INSERT INTO processing_run
       (processing_id, tool_name, tool_version, correction_type, status, message)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = list(
      proc_id, 'authors_python_script', 'Lao & Ying 2026 (custom Python)',
      'plate_mean_t0', 'completed',
      paste0('K and r from Growth_parameters.xlsx (Lao & Ying 2026). ',
             'indicator_K: 0=valid, 1=biological bias, 3=replicate outlier. ',
             'indicator_r: 0=valid, 2=computational bias, 3=replicate outlier.')
    )
  )

  # medium stays in gp; only jwid comes from meta to avoid column collision
  gp_full <- merge(gp, meta[, c('curve_id', 'jwid')], by = 'curve_id')
  gp_full$dataset_id <- paste0('keio_', tolower(gp_full$jwid), '_', tolower(gp_full$medium))
  gp_full <- gp_full[order(gp_full$dataset_id, gp_full$curve_id), ]
  gp_full$rep_n   <- ave(seq_len(nrow(gp_full)), gp_full$dataset_id,
                          FUN = function(x) seq_along(x))
  gp_full$well_id <- paste0(gp_full$dataset_id, '__plate_1__R', gp_full$rep_n)

  imported_ids <- DBI::dbGetQuery(con,
    "SELECT dataset_id FROM dataset WHERE dataset_id LIKE 'keio_%'")$dataset_id
  gp_full <- gp_full[gp_full$dataset_id %in% imported_ids, ]

  if (nrow(gp_full) == 0L) {
    .msg('No matching keio datasets found in DB. Run keio_import_all() first.')
    return(invisible(NULL))
  }

  metrics <- rbind(
    data.frame(processing_id = proc_id, dataset_id = gp_full$dataset_id,
               well_id = gp_full$well_id, metric_name = 'K',
               metric_value = as.numeric(gp_full$K),    metric_unit = 'OD600',
               model_name = 'authors', stringsAsFactors = FALSE),
    data.frame(processing_id = proc_id, dataset_id = gp_full$dataset_id,
               well_id = gp_full$well_id, metric_name = 'r',
               metric_value = as.numeric(gp_full$r),    metric_unit = '1/h',
               model_name = 'authors', stringsAsFactors = FALSE),
    data.frame(processing_id = proc_id, dataset_id = gp_full$dataset_id,
               well_id = gp_full$well_id, metric_name = 'indicator_K',
               metric_value = as.numeric(gp_full$indicator_K), metric_unit = NA_character_,
               model_name = 'authors', stringsAsFactors = FALSE),
    data.frame(processing_id = proc_id, dataset_id = gp_full$dataset_id,
               well_id = gp_full$well_id, metric_name = 'indicator_r',
               metric_value = as.numeric(gp_full$indicator_r), metric_unit = NA_character_,
               model_name = 'authors', stringsAsFactors = FALSE)
  )
  metrics$metric_id <- paste0('keio_m_', seq_len(nrow(metrics)))

  DBI::dbWriteTable(con, 'derived_metric', metrics, append = TRUE)
  .msg(sprintf('Done. Inserted %d metric rows across %d datasets.',
               nrow(metrics), length(unique(gp_full$dataset_id))))
  invisible(metrics)
}

# ── Gene-level external link helpers ─────────────────────────────────────────
# Called from app.R Links tab when detail_dataset_id() starts with "keio_".

keio_uniprot_url <- function(gene_name) {
  paste0(
    'https://www.uniprot.org/uniprotkb?query=',
    'gene_exact%3A', utils::URLencode(gene_name, reserved = TRUE),
    '+AND+taxonomy_id%3A83333+AND+reviewed%3Atrue'
  )
}

# CGSC/ECGRC — strain page by JWID number (strip leading zeros after "JW")
# URL pattern: https://ecgrc.net/CGSC/Strain.php?ID=<number>
# Verify the URL format at https://ecgrc.net/CGSC/index.php if it changes.
keio_cgsc_url <- function(jwid) {
  num <- as.integer(sub('^JW0*', '', toupper(trimws(jwid))))
  paste0('https://ecgrc.net/CGSC/Strain.php?ID=', num)
}

# KEGG E. coli gene entry — reliable fallback
keio_kegg_url <- function(gene_name) {
  paste0('https://www.genome.jp/dbget-bin/www_bget?eco:', gene_name)
}
