# R/import/shimoganadig_import.R
# ──────────────────────────────────────────────────────────────────────────────
# Import: Shimoga Nadig, Gross, Bollenbach & Ansmann (2025) Open Biology 15:240301
# "Trace elements increase replicability of microbial growth"
# DOI: 10.1098/rsob.240301  |  Zenodo: 12746431 (CC BY 4.0)
#
# 11 strains (+ O. anthropi, excluded by the authors for noise) grown in high
# replication in M9 + 4 g/L glucose with / without 1 % ATCC MD-TMS trace elements.
# BioTek Synergy H1, 96-well (Thermo 260860), 200 µl, 30 °C, OD600 ~every 22 min.
#
# Source layout (OD_data.tar.gz → Series_1, 2, 4, 5):
#   one Gen5 CSV export per plate read: '<datetime>_<run>_GA_GrowthCurve_<plate>.csv'
#   plates 1–10 are replicate plates; each plate COLUMN is one strain × condition
#   (map below, transcribed from the authors' experiments.py); rows A–H = replicates.
#   Series 4 and 5 use the "bottle mixing" set-up (100 µl from the main bottle +
#   100 µl from one of eight bottles).
#   Series_5: the top-level CSVs are the first 65 reads only; the subfolder
#   230228_8row_12column_set2 holds the complete run (174 reads, identical readings
#   for the overlap) and is the one imported here.
#
# Design: 1 series × 1 strain = 1 dataset (both trace conditions side by side),
#   10 plates × 2 columns × 8 rows = 160 curves. Plate columns outside the authors'
#   map (Series 5 cols 1–3, 6, 9, 10) carry no metadata and are not imported.
# Values: raw OD600 as recorded; time = read timestamp − first read of that plate.
#
# Usage:
#   source('R/import/shimoganadig_import.R')
#   shimoganadig_import_all(con, source_dir = '/path/to/ShimogaNadig_2025_trace_elements')
# ──────────────────────────────────────────────────────────────────────────────

SN_DOI        <- '10.1098/rsob.240301'
SN_YEAR       <- 2025L
SN_AUTHORS    <- 'Shimoga Nadig A, Gross R, Bollenbach T, Ansmann G'
SN_LICENSE    <- 'CC BY 4.0'
SN_SOURCE_URL <- 'https://zenodo.org/records/12746431'
SN_MEDIUM     <- 'M9 + 4 g/L glucose (+ 2 mM MgSO4, 0.1 mM CaCl2)'
SN_TRACE      <- 'trace elements (ATCC MD-TMS)'

# (series, column) → strain; transcribed from experiments.py (pos_to_experiment)
.sn_map <- function() {
  s <- function(series, cols, strain_id, species, trace)
    data.frame(series = series, col = cols, strain_id = strain_id, species = species,
               has_trace = trace, stringsAsFactors = FALSE)
  pair <- function(series, c1, strain_id, species, first_has_trace)
    rbind(s(series, c1, strain_id, species, first_has_trace),
          s(series, c1 + 1L, strain_id, species, !first_has_trace))
  rbind(
    pair(1L,  1L, 'UTI29',   'Rhizobium radiobacter',      TRUE),
    pair(1L,  3L, 'UTI5',    'Pseudomonas fluorescens',    TRUE),
    pair(1L,  5L, 'CEent-1', 'Enterobacter cloacae',       TRUE),
    pair(1L,  7L, 'UTI74',   'Staphylococcus capitis',     TRUE),
    pair(1L,  9L, 'MYb11',   'Pseudomonas lurida',         TRUE),
    pair(1L, 11L, 'MG1655',  'E. coli',                    TRUE),
    pair(2L,  1L, 'MYb49',   'Ochrobactrum anthropi',      TRUE),
    pair(2L,  3L, 'MYb181',  'Sphingobacterium faecium',   TRUE),
    pair(2L,  5L, 'JuB134',  'Sphingomonas molluscorum',   TRUE),
    pair(2L,  7L, 'UTI #57', 'Staphylococcus auricularis', TRUE),
    pair(2L,  9L, 'UTI #45', 'Staphylococcus simulans',    TRUE),
    pair(2L, 11L, 'MYb121',  'Erwinia billingiae',         TRUE),
    pair(4L,  1L, 'MG1655',  'E. coli',                    FALSE),
    pair(4L,  3L, 'MYb11',   'Pseudomonas lurida',         FALSE),
    pair(4L,  5L, 'UTI74',   'Staphylococcus capitis',     FALSE),
    pair(4L,  7L, 'UTI29',   'Rhizobium radiobacter',      FALSE),
    pair(4L,  9L, 'UTI5',    'Pseudomonas fluorescens',    FALSE),
    pair(4L, 11L, 'CEent-1', 'Enterobacter cloacae',       FALSE),
    pair(5L,  4L, 'UTI #57', 'Staphylococcus auricularis', FALSE),
    pair(5L,  7L, 'UTI #45', 'Staphylococcus simulans',    FALSE),
    pair(5L, 11L, 'MYb121',  'Erwinia billingiae',         FALSE))
}

# NCBI taxon IDs (R. radiobacter → Agrobacterium tumefaciens 358;
# O. anthropi → Brucella anthropi 529)
SN_TAXON <- c('Rhizobium radiobacter' = '358', 'Pseudomonas fluorescens' = '294',
              'Enterobacter cloacae' = '550', 'Staphylococcus capitis' = '29388',
              'Pseudomonas lurida' = '244566', 'E. coli' = '562',
              'Ochrobactrum anthropi' = '529', 'Sphingobacterium faecium' = '34087',
              'Sphingomonas molluscorum' = '418184', 'Staphylococcus auricularis' = '29379',
              'Staphylococcus simulans' = '1286', 'Erwinia billingiae' = '182337')

# Wells the authors excluded after manual inspection (series, plate, row, col);
# transcribed from experiments.py (bad_wells)
SN_BAD_WELLS <- c('1_10_F3', '2_1_D8', '2_1_E7', '2_2_B2', '2_2_E12', '2_2_G7', '2_3_C4',
                  '2_5_C1', '2_6_D4', '2_4_F4', '2_5_F4', '2_5_G4', '2_5_H4', '2_7_A4',
                  '2_7_B4', '2_7_E1', '2_7_H4', '2_8_D4', '2_8_E1', '2_8_F1', '2_8_G1',
                  '2_9_C4', '2_9_C8', '2_9_D1', '2_9_D4', '2_9_F7', '2_9_H4', '2_10_D8',
                  '2_10_E6', '2_10_F6')

# Parse one Gen5 single-read CSV → list(datetime, 8 × 12 OD matrix)
.sn_read_plate <- function(path) {
  x <- sub('\r$', '', readLines(path, encoding = 'latin1', warn = FALSE))
  d <- sub('^Date\t', '', grep('^Date\t', x, value = TRUE))
  t <- sub('^Time\t', '', grep('^Time\t', x, value = TRUE))
  i <- which(x == 'Results')
  if (length(d) != 1L || length(t) != 1L || length(i) != 1L)
    stop('Unexpected Gen5 export layout: ', path)
  rows <- strsplit(x[i + 2:9], '\t')
  if (!identical(vapply(rows, `[`, '', 1L), LETTERS[1:8])) stop('Bad row labels: ', path)
  od <- t(vapply(rows, function(r) as.numeric(r[2:13]), numeric(12)))
  if (anyNA(od)) stop('Non-numeric reading in ', path)
  dimnames(od) <- list(LETTERS[1:8], 1:12)
  list(datetime = as.POSIXct(paste(d, trimws(t)), format = '%m/%d/%Y %I:%M:%S %p', tz = 'UTC'),
       od = od)
}

# All reads of one series → long data.frame (series, plate, well, row, col, datetime, value)
.sn_read_series <- function(dir) {
  files <- list.files(dir, pattern = 'GrowthCurve_[0-9]+\\.csv$', full.names = TRUE)
  out <- lapply(files, function(f) {
    p <- .sn_read_plate(f)
    data.frame(plate = as.integer(sub('.*GrowthCurve_([0-9]+)\\.csv$', '\\1', f)),
               row = rep(LETTERS[1:8], 12), col = rep(1:12, each = 8),
               datetime = p$datetime, value = as.vector(p$od), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

shimoganadig_import_all <- function(
    con,
    source_dir  = NULL,
    parquet_dir = 'data/parquet/raw_measurements',
    overwrite   = FALSE,
    verbose     = TRUE
) {
  if (is.null(source_dir)) stop('source_dir is required: folder holding OD_data_extracted/')
  requireNamespace('DBI'); requireNamespace('arrow'); requireNamespace('data.table')
  .msg <- function(...) if (verbose) message('[shimoganadig] ', ...)
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)
  for (sql in c("ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS dataset_id TEXT",
                "ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS gene_category TEXT"))
    tryCatch(DBI::dbExecute(con, sql), error = function(e) NULL)

  n_exist <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM dataset WHERE dataset_id LIKE 'shimoganadig_2025_%'")$n
  if (n_exist > 0L && !overwrite) {
    .msg(n_exist, ' datasets already present — pass overwrite = TRUE to re-import')
    return(invisible(NULL))
  }

  od_dir <- file.path(source_dir, 'OD_data_extracted')
  dirs <- c(`1` = 'Series_1', `2` = 'Series_2', `4` = 'Series_4',
            `5` = 'Series_5/230228_8row_12column_set2')
  map <- .sn_map()
  all_ds <- list(); all_pl <- list(); all_wm <- list(); all_reg <- list()

  for (sr in names(dirs)) {
    series <- as.integer(sr)
    raw <- .sn_read_series(file.path(od_dir, dirs[[sr]]))
    .msg(sprintf('Series %d: %d reads', series, nrow(raw) / 96L))

    if (series == 5L) {   # overlap with the truncated top-level copy must be identical
      top <- .sn_read_series(file.path(od_dir, 'Series_5'))
      chk <- merge(top, raw, by = c('plate', 'row', 'col', 'datetime'))
      if (nrow(chk) != nrow(top) || any(chk$value.x != chk$value.y))
        stop('Series_5 top-level reads differ from the complete run in the subfolder')
    }

    # time since first read of each plate
    t0 <- tapply(raw$datetime, raw$plate, min)
    raw$time_h <- as.numeric(raw$datetime - as.POSIXct(t0[as.character(raw$plate)],
                                                       origin = '1970-01-01', tz = 'UTC'),
                             units = 'hours')

    smap <- map[map$series == series, ]
    for (sid in unique(smap$strain_id)) {
      m   <- smap[smap$strain_id == sid, ]
      did <- sprintf('shimoganadig_2025_s%d_%s', series, gsub('[^a-z0-9]', '', tolower(sid)))
      bottle <- series %in% c(4L, 5L)
      d <- raw[raw$col %in% m$col, ]
      d$plate_id <- paste0(did, '__plate_', d$plate)
      d$well     <- paste0(d$row, d$col)
      d$well_id  <- paste0(d$plate_id, '__', d$well)
      d <- d[order(d$plate, d$col, d$row, d$time_h), ]
      pq <- data.frame(dataset_id = did, plate_id = d$plate_id, well_id = d$well_id,
                       well = d$well, time_h = d$time_h, signal_type = 'OD600',
                       value = d$value, stringsAsFactors = FALSE)
      pq_path <- file.path(parquet_dir, paste0(did, '.parquet'))
      arrow::write_parquet(pq, pq_path)

      wells <- unique(d[, c('plate', 'plate_id', 'well', 'well_id', 'row', 'col')])
      wells <- merge(wells, m[, c('col', 'has_trace')], by = 'col')
      bad <- wells[paste(series, wells$plate, paste0(wells$row, wells$col), sep = '_') %in%
                     SN_BAD_WELLS, ]
      sp  <- m$species[1]
      cols_txt <- sprintf('column %d = with trace elements, column %d = without',
                          m$col[m$has_trace], m$col[!m$has_trace])

      all_ds[[did]] <- data.frame(
        dataset_id = did, raw_file = paste0('OD_data.tar.gz/', dirs[[sr]]),
        title = sprintf('%s %s ± trace elements, M9 glucose%s (Shimoga Nadig 2025, series %d)',
                        sp, sid, if (bottle) ', bottle mixing' else '', series),
        doi = SN_DOI, year = SN_YEAR, authors = SN_AUTHORS, license = SN_LICENSE,
        source_url = SN_SOURCE_URL, time_unit = 'hours',
        notes = paste0(
          'Raw OD600 as recorded (no blank wells; authors subtract the median of per-curve ',
          'minima for their analysis). 10 replicate plates × 8 wells per condition; ', cols_txt, '. ',
          if (bottle) 'Bottle-mixing set-up: 100 µl from the main bottle + 100 µl from one of eight bottles. ' else '',
          if (series == 5L) 'Complete run (174 reads); the authors analysed the first 65 reads. ' else '',
          if (sid == 'MYb49') 'Excluded from the authors\' analysis: grew badly with high measurement noise. ' else '',
          if (nrow(bad)) paste0('Wells excluded by the authors after manual inspection: ',
            paste(sprintf('plate %d %s', bad$plate, bad$well), collapse = ', '), '. ') else '',
          'Trace-element supplement: 1 % ATCC MD-TMS.'),
        stringsAsFactors = FALSE)
      all_pl[[did]] <- data.frame(
        plate_id = paste0(did, '__plate_', 1:10), dataset_id = did, plate_format = '96',
        plate_reader = 'Synergy H1 (BioTek)', wavelength_nm = 600, temperature_c = 30,
        shake_freq_rpm = NA_real_, stringsAsFactors = FALSE)
      all_wm[[did]] <- data.frame(
        well_id = wells$well_id, plate_id = wells$plate_id, well = wells$well,
        species = sp, taxon_id = SN_TAXON[[sp]], strain = sid, medium_state = 'liquid',
        medium_type = SN_MEDIUM,
        perturbation_type = ifelse(wells$has_trace, 'chemical', 'none'),
        perturbation = ifelse(wells$has_trace, SN_TRACE, NA_character_),
        concentration = ifelse(wells$has_trace, 1, NA_real_),
        concentration_unit = ifelse(wells$has_trace, '% v/v', NA_character_),
        is_blank = FALSE, is_control = !wells$has_trace,
        replicate = match(wells$row, LETTERS[1:8]) + 8L * (wells$plate - 1L),
        dataset_id = did, gene_category = NA_character_, stringsAsFactors = FALSE)
      all_reg[[did]] <- data.frame(
        dataset_id = did, storage_type = 'parquet', path = pq_path, n_rows = nrow(pq),
        n_wells = nrow(wells), signal_type = 'OD600', measurement_unit = 'OD',
        stringsAsFactors = FALSE)
    }
  }

  bind <- function(x) data.table::rbindlist(x, use.names = TRUE)
  .msg(sprintf('Writing %d datasets, %d curves to DB...', length(all_ds), nrow(bind(all_wm))))
  DBI::dbWithTransaction(con, {
    if (n_exist > 0L) for (sql in c(
      "DELETE FROM well_metadata WHERE plate_id LIKE 'shimoganadig\\_2025\\_%' ESCAPE '\\'",
      "DELETE FROM raw_measurement_registry WHERE dataset_id LIKE 'shimoganadig\\_2025\\_%' ESCAPE '\\'",
      "DELETE FROM dataset_conditions WHERE dataset_id LIKE 'shimoganadig\\_2025\\_%' ESCAPE '\\'",
      "DELETE FROM plate WHERE dataset_id LIKE 'shimoganadig\\_2025\\_%' ESCAPE '\\'",
      "DELETE FROM dataset WHERE dataset_id LIKE 'shimoganadig\\_2025\\_%' ESCAPE '\\'"
    )) DBI::dbExecute(con, sql)
    DBI::dbAppendTable(con, 'dataset', bind(all_ds))
    DBI::dbAppendTable(con, 'plate', bind(all_pl))
    DBI::dbAppendTable(con, 'well_metadata', bind(all_wm))
    DBI::dbAppendTable(con, 'raw_measurement_registry', bind(all_reg))
  })
  qres_sync_conditions(con, names(all_ds))
  .msg(sprintf('Done: %d datasets', length(all_ds)))
  invisible(names(all_ds))
}
