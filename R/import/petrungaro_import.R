# R/import/petrungaro_import.R
# ──────────────────────────────────────────────────────────────────────────────
# Import: Petrungaro G, Fink T, Fernando B, Ansmann G, Bollenbach T (2026)
# "Function-specific epistasis shapes evolutionary trajectories towards antibiotic
# resistance". Nature Communications 17:7564. DOI 10.1038/s41467-026-76025-1
# Data: Zenodo 10.5281/zenodo.21508963 (v2, CC BY 4.0) — fsepi_data.zip
#
# Design (Methods): automated high-throughput morbidostat. 96-well plates, 200 µl LB,
# 30 °C, Liconic incubator 500 rpm, OD600 (BioTek Synergy H1) every 24 min; each well
# is diluted to OD 0.01 every ~4.2 h and the antibiotic concentration is updated at
# each dilution to keep ~50 % growth inhibition. Each well = one evolving lineage;
# each interval between two dilutions = one transfer cycle (a short batch curve at a
# constant drug concentration). Cycle 0 = growth before the first dilution (no drug).
#
# Runs (paper Table 1)                     plates  mother plates
#   NIT_evo  nitrofurantoin   21 Jun 2021    9     GPm1 (plates 1–3), GPm2 (4–6), GPm3 (7–9)
#   TMP_evo  trimethoprim     12 Aug 2021    9     idem
#   MEC_evo  mecillinam       20 May 2022    9     idem
#   inhibitors_evo  MEC/NIT/TMP/none by row, 24 Apr 2023, 6 plates:
#     plates 1–3: ΔdnaJ ΔdnaK Δtig + 7 UTI isolates  → telaprevir (DnaK inhibitor) 70 µg/ml
#     plates 4–6: ΔacrA ΔacrB ΔmarA ΔtolC + 6 UTI    → domperidone (AcrAB–TolC) 25 µg/ml
#     plates 2 and 5 share reservoir set LB_3 = "one plate each without inhibitor"
#     (inhibitor ↔ plate assignment inferred from Plates_info + Methods; to confirm)
#
# Design in PhaMiG: 1 run × 1 mother plate = 1 dataset (its 3 replicate plates);
#   perturbation_type = 'evolution', perturbation = selective drug (+ co-treatment);
#   per-cycle drug concentration, timing and background → transfer_cycle;
#   Parquet carries an integer `cycle` column (see qres_split_transfer_cycles()).
#
# OD values:
#   inhibitors_evo  raw OD from raw_data/OD_all_data_plate*.csv (complete, incl. media wells)
#   TMP_evo, MEC_evo raw OD = deposited background-subtracted OD + that cycle's background
#                    (background_values_plate*.csv; identity verified exactly on
#                    inhibitors_evo, where both versions exist). Cells the authors
#                    blanked (mostly media-only wells and readings ≤ background) are absent.
#   NIT_evo          background-subtracted only (no background file in the deposit)
#
# Strain names: single-drug runs use plate_layouts codes (s1…s86) + the mother-plate
# lists strain_names_plate_GPm*.csv (v2-corrected). TF_plate_layouts disagree in one
# well (GPm1 s77: 'tsx' → corrected 'hupA') and spell 'fhua' for fhuA.
#
# Usage:
#   source('R/import/petrungaro_import.R')
#   petrungaro_import_all(con, zip_path = '../Petrungaro_2026_resistance_evolution/fsepi_data_v2.zip')
# ──────────────────────────────────────────────────────────────────────────────

PG_DOI        <- '10.1038/s41467-026-76025-1'
PG_YEAR       <- 2026L
PG_AUTHORS    <- 'Petrungaro G, Fink T, Fernando B, Ansmann G, Bollenbach T'
PG_LICENSE    <- 'CC BY 4.0'
PG_SOURCE_URL <- 'https://doi.org/10.5281/zenodo.21508963'
PG_MEDIUM     <- 'LB (Sigma L3022)'
PG_DRUGS      <- c(NIT = 'Nitrofurantoin', TMP = 'Trimethoprim', MEC = 'Mecillinam')

PG_RUNS <- list(
  nit_evo        = list(dir = 'Results_NIT_9plates_21062021', drug = 'NIT', label = 'NIT_evo'),
  tmp_evo        = list(dir = 'Results_TMP_9plates_12082021', drug = 'TMP', label = 'TMP_evo'),
  mec_evo        = list(dir = 'Results_MEC_9plates_20052022', drug = 'MEC', label = 'MEC_evo'),
  inhibitors_evo = list(dir = 'Results_MEC_NIT_TMP_6plates_24042023', drug = NA, label = 'inhibitors_evo'))

# inhibitors_evo: plate → co-treatment (see header)
PG_INHIBITOR <- c(`21721` = 'telaprevir 70 µg/ml', `21722` = NA, `21723` = 'telaprevir 70 µg/ml',
                  `21724` = 'domperidone 25 µg/ml', `21725` = NA, `21726` = 'domperidone 25 µg/ml')

.pg_grid <- function(path) {                 # plate-layout CSV → named vector A1…H12
  g <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, colClasses = 'character')
  v <- unlist(lapply(seq_len(nrow(g)), function(i) setNames(trimws(unlist(g[i, -1])),
                                                            paste0(g[i, 1], names(g)[-1]))))
  v
}

.pg_tsv <- function(path) {
  utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE, na.strings = '')
}

.pg_strain <- function(name) {
  if (name == 'LB') return(list(species = '', taxon = '', strain = '', blank = TRUE))
  if (grepl('^lacA[0-9]*$', name))
    return(list(species = 'E. coli', taxon = '562', strain = 'BW25113 ΔlacA (Keio; reference)', blank = FALSE))
  if (grepl('^RG2_', name))
    return(list(species = 'E. coli', taxon = '562', strain = paste('UTI isolate', name), blank = FALSE))
  list(species = 'E. coli', taxon = '562', strain = sprintf('BW25113 Δ%s (Keio)', name), blank = FALSE)
}

petrungaro_import_all <- function(
    con,
    zip_path    = NULL,
    parquet_dir = 'data/parquet/raw_measurements',
    overwrite   = FALSE,
    verbose     = TRUE
) {
  if (is.null(zip_path) || !file.exists(zip_path)) stop('zip_path to fsepi_data.zip (Zenodo v2) is required')
  requireNamespace('DBI'); requireNamespace('arrow'); requireNamespace('data.table')
  .msg <- function(...) if (verbose) message('[petrungaro] ', ...)
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)
  for (sql in c("ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS dataset_id TEXT",
                "ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS gene_category TEXT"))
    tryCatch(DBI::dbExecute(con, sql), error = function(e) NULL)
  qres_init_schema(con)                       # creates transfer_cycle if missing

  n_exist <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM dataset WHERE dataset_id LIKE 'petrungaro_2026_%'")$n
  if (n_exist > 0L && !overwrite) {
    .msg(n_exist, ' datasets already present — pass overwrite = TRUE to re-import')
    return(invisible(NULL))
  }

  src <- file.path(tempdir(), 'petrungaro_fsepi')
  unlink(src, recursive = TRUE)
  utils::unzip(zip_path, exdir = src)
  root <- file.path(src, 'fsepi_data')

  # mother plates (v2-corrected) : code → gene
  mother <- lapply(c(GPm1 = 'strain_names_plate_GPm1bkpcorrected.csv',
                     GPm2 = 'strain_names_plate_GPm2.csv', GPm3 = 'strain_names_plate_GPm3.csv'),
    function(f) { m <- utils::read.csv(file.path(root, f), stringsAsFactors = FALSE)
                  setNames(trimws(m[[2]]), trimws(m[[1]])) })

  all_ds <- list(); all_pl <- list(); all_wm <- list(); all_reg <- list(); all_tc <- list()

  for (run_key in names(PG_RUNS)) {
    run  <- PG_RUNS[[run_key]]; rdir <- file.path(root, run$dir)
    info_dir <- file.path(rdir, 'CurrentExperiment_info')
    pinfo <- utils::read.table(list.files(info_dir, '^Plates_info', full.names = TRUE),
                               header = TRUE, stringsAsFactors = FALSE)
    cexp <- readLines(list.files(info_dir, '^current_experiment', full.names = TRUE), warn = FALSE)
    cexp <- paste(trimws(cexp[nzchar(trimws(cexp)) & cexp != 'OUTPUT']), collapse = '; ')
    plates <- as.character(pinfo$Plate)
    .msg(run$label, ': ', length(plates), ' plates')

    plate_rows <- list()
    for (i in seq_along(plates)) {
      p   <- plates[i]
      od  <- .pg_tsv(file.path(rdir, 'data', sprintf('ODdata_bg_sub_with_exptime_plate%s.csv', p)))
      ab  <- .pg_tsv(file.path(rdir, 'data', sprintf('ABconc_with_exptime_plate%s.csv', p)))
      wells <- paste0(rep(LETTERS[1:8], each = 12), 1:12)
      dil_t <- as.numeric(ab[['time[h]']])
      read_t <- as.numeric(od[['time[h]']])
      cyc   <- findInterval(read_t, dil_t) - 1L                 # cycle k: t_k <= t < t_(k+1)
      bg_f  <- file.path(rdir, 'data', sprintf('background_values_plate%s.csv', p))
      raw_f <- file.path(rdir, 'raw_data', sprintf('OD_all_data_plate%s.csv', p))
      bg    <- if (file.exists(bg_f)) { b <- .pg_tsv(bg_f); setNames(b$bg, b$dilution) } else NULL
      if (file.exists(raw_f)) {
        raw <- .pg_tsv(raw_f)
        if (!identical(raw[[1]], od[[1]])) stop('Read index of raw and bg-subtracted files differ: plate ', p)
        vals <- as.matrix(raw[, wells]); status <- 'raw'
      } else if (!is.null(bg)) {
        vals <- as.matrix(od[, wells]) + bg[as.character(cyc)]; status <- 'reconstructed'
      } else {
        vals <- as.matrix(od[, wells]); status <- 'bg_subtracted'
      }
      if (anyNA(cyc) || any(cyc < 0)) stop('Reads before the first time stamp in plate ', p)

      # strain names per well
      code <- .pg_grid(file.path(rdir, 'plate_layouts', sprintf('plate%d_%s_layout_%s.csv', i, p,
                       sub('.*_', '', run$dir))))
      tf   <- .pg_grid(file.path(rdir, 'TF_plate_layouts_strain_names', sprintf('plate%d_%s_layout_%s.csv', i, p,
                       sub('.*_', '', run$dir))))
      if (is.na(run$drug)) {
        names_w <- tf[wells]; mp <- if (i <= 3) 'telaprevir set' else 'domperidone set'
        drugs   <- .pg_grid(file.path(rdir, 'plate_layouts', sprintf('antibiotics_plate%d_%s_layout_%s.csv', i, p,
                            sub('.*_', '', run$dir))))[wells]
        cotreat <- unname(PG_INHIBITOR[p])
      } else {
        mp <- paste0('GPm', ceiling(i / 3)); mmap <- c(mother[[mp]], lacA = 'lacA', LB = 'LB')
        names_w <- unname(mmap[code[wells]])
        if (anyNA(names_w)) stop('Unmapped layout code on plate ', p)
        drugs <- rep(run$drug, 96); cotreat <- NA_character_
      }
      plate_rows[[i]] <- list(p = p, i = i, mp = mp, names = names_w, drugs = drugs, cotreat = cotreat,
                              vals = vals, read_t = read_t, cyc = cyc, dil_t = dil_t, ab = ab, bg = bg,
                              status = status, tf_diff = sum(tolower(tf[wells]) != tolower(names_w) &
                                                              !grepl('^lacA', tf[wells])))
    }

    # one dataset per mother plate
    for (mp in unique(vapply(plate_rows, `[[`, '', 'mp'))) {
      prs <- Filter(function(x) x$mp == mp, plate_rows)
      did <- sprintf('petrungaro_2026_%s_%s', run_key, gsub('[^a-z0-9]+', '_', tolower(mp)))
      long <- list(); wm <- list(); tc <- list()
      for (k in seq_along(prs)) {
        pr  <- prs[[k]]; pid <- sprintf('%s__plate_%d', did, pr$i)
        wells <- colnames(pr$vals)
        d <- data.frame(dataset_id = did, plate_id = pid,
                        well_id = paste0(pid, '__', rep(wells, each = nrow(pr$vals))),
                        well = rep(wells, each = nrow(pr$vals)),
                        time_h = rep(pr$read_t, length(wells)), signal_type = 'OD600',
                        value = as.numeric(pr$vals), cycle = rep(pr$cyc, length(wells)),
                        stringsAsFactors = FALSE)
        long[[k]] <- d[!is.na(d$value), ]
        st <- lapply(pr$names, .pg_strain)
        drug_nm <- ifelse(pr$drugs == 'NOAB', 'no antibiotic', PG_DRUGS[pr$drugs])
        pert <- if (is.na(pr$cotreat)) drug_nm else paste0(drug_nm, ' + ', pr$cotreat)
        blank <- vapply(st, `[[`, TRUE, 'blank')
        wm[[k]] <- data.frame(
          well_id = paste0(pid, '__', wells), plate_id = pid, well = wells,
          species = vapply(st, `[[`, '', 'species'), taxon_id = vapply(st, `[[`, '', 'taxon'),
          strain = vapply(st, `[[`, '', 'strain'), medium_state = 'liquid', medium_type = PG_MEDIUM,
          perturbation_type = ifelse(blank, 'none', 'evolution'),
          perturbation = ifelse(blank, NA, pert), concentration = NA_real_,
          concentration_unit = NA_character_, is_blank = blank,
          is_control = !blank & pr$drugs == 'NOAB', replicate = (pr$i - 1L) %% 3L + 1L,
          dataset_id = did, gene_category = NA_character_, stringsAsFactors = FALSE)
        # transfer cycles: one row per well × cycle
        ncyc <- length(pr$dil_t)
        n_reads <- tabulate(pr$cyc + 1L, nbins = ncyc)
        end_t <- c(pr$dil_t[-1], max(pr$read_t))
        conc <- as.matrix(pr$ab[, wells])
        tc[[k]] <- data.frame(
          dataset_id = did, well_id = paste0(pid, '__', rep(wells, each = ncyc)),
          cycle = rep(seq_len(ncyc) - 1L, length(wells)),
          start_time_h = rep(pr$dil_t, length(wells)), end_time_h = rep(end_t, length(wells)),
          n_reads = rep(n_reads, length(wells)),
          drug = rep(ifelse(pr$drugs == 'NOAB', NA, PG_DRUGS[pr$drugs]), each = ncyc),
          drug_conc = as.numeric(conc), drug_conc_unit = 'µg/ml', co_treatment = pr$cotreat,
          background_od = if (is.null(pr$bg)) NA_real_ else rep(unname(pr$bg[as.character(seq_len(ncyc) - 1L)]), length(wells)),
          stringsAsFactors = FALSE)
      }
      long <- data.table::rbindlist(long)
      pq_path <- file.path(parquet_dir, paste0(did, '.parquet'))
      arrow::write_parquet(long, pq_path)
      status <- unique(vapply(prs, `[[`, '', 'status'))
      wm <- data.table::rbindlist(wm)
      genes <- sort(unique(wm$strain[grepl('Keio\\)$', wm$strain)]))
      all_ds[[did]] <- data.frame(
        dataset_id = did, raw_file = paste0('fsepi_data.zip/', run$dir),
        title = sprintf('E. coli experimental evolution (morbidostat), %s, %s (%d lineages; Petrungaro 2026)',
                        run$label, if (is.na(run$drug)) paste0('MEC/NIT/TMP, ', mp) else
                          paste0(PG_DRUGS[[run$drug]], ', mother plate ', mp), sum(!wm$is_blank)),
        doi = PG_DOI, year = PG_YEAR, authors = PG_AUTHORS, license = PG_LICENSE,
        source_url = PG_SOURCE_URL, time_unit = 'hours',
        notes = paste0(
          'Morbidostat evolution experiment (', run$label, '). Each well is one lineage; the culture is ',
          'diluted to OD 0.01 about every 4.2 h and the antibiotic concentration is updated at each ',
          'dilution (per-cycle concentration, timing and background in table transfer_cycle; Parquet ',
          'column `cycle`). ', length(prs), ' replicate plates of mother plate ', mp, '. ',
          switch(status[1],
            raw = 'OD600: raw values from raw_data/OD_all_data. ',
            reconstructed = 'OD600: raw values reconstructed as deposited background-subtracted OD + per-cycle background (exact identity verified on inhibitors_evo); cells blanked by the authors are absent. ',
            bg_subtracted = 'OD600: background-subtracted as deposited (no background values in the deposit). '),
          if (!is.na(run$drug) && mp == 'GPm1') 'Well H2 (mother plate GPm1, code s77) is ΔhupA per the v2-corrected strain list; per-plate layout files still say Δtsx. ' else '',
          if (is.na(run$drug)) 'Rows A–B mecillinam, C–D nitrofurantoin, E–F trimethoprim, G–H no antibiotic. Inhibitor assignment (telaprevir with the ΔdnaK set, domperidone with the efflux set; plates 2 and 5 without inhibitor) inferred from reservoir sets and Methods. ' else '',
          'Media-only wells (contamination controls) are blanks. Nine populations that died in the ',
          'study were excluded by the authors (Supplementary Table 3). Run parameters: ', cexp, '.'),
        stringsAsFactors = FALSE)
      all_pl[[did]] <- data.frame(
        plate_id = sprintf('%s__plate_%d', did, vapply(prs, `[[`, 0L, 'i')), dataset_id = did,
        plate_format = '96', plate_reader = 'Synergy H1 (BioTek)', wavelength_nm = 600,
        temperature_c = 30, shake_freq_rpm = 500, stringsAsFactors = FALSE)
      all_wm[[did]] <- wm
      all_tc[[did]] <- data.table::rbindlist(tc)
      all_reg[[did]] <- data.frame(
        dataset_id = did, storage_type = 'parquet', path = pq_path, n_rows = nrow(long),
        n_wells = nrow(wm), signal_type = 'OD600', measurement_unit = 'OD', stringsAsFactors = FALSE)
      .msg(sprintf('  %s: %d wells, %d reads, %d cycle rows, OD %s, TF-layout differences %d',
                   did, nrow(wm), nrow(long), nrow(all_tc[[did]]), status[1],
                   sum(vapply(prs, `[[`, 0L, 'tf_diff'))))
    }
  }

  bind <- function(x) data.table::rbindlist(x, use.names = TRUE)
  DBI::dbWithTransaction(con, {
    if (n_exist > 0L) for (sql in c(
      "DELETE FROM transfer_cycle WHERE dataset_id LIKE 'petrungaro\\_2026\\_%' ESCAPE '\\'",
      "DELETE FROM well_metadata WHERE plate_id LIKE 'petrungaro\\_2026\\_%' ESCAPE '\\'",
      "DELETE FROM raw_measurement_registry WHERE dataset_id LIKE 'petrungaro\\_2026\\_%' ESCAPE '\\'",
      "DELETE FROM dataset_conditions WHERE dataset_id LIKE 'petrungaro\\_2026\\_%' ESCAPE '\\'",
      "DELETE FROM plate WHERE dataset_id LIKE 'petrungaro\\_2026\\_%' ESCAPE '\\'",
      "DELETE FROM dataset WHERE dataset_id LIKE 'petrungaro\\_2026\\_%' ESCAPE '\\'"
    )) DBI::dbExecute(con, sql)
    DBI::dbAppendTable(con, 'dataset', bind(all_ds))
    DBI::dbAppendTable(con, 'plate', bind(all_pl))
    DBI::dbAppendTable(con, 'well_metadata', bind(all_wm))
    DBI::dbAppendTable(con, 'raw_measurement_registry', bind(all_reg))
    DBI::dbAppendTable(con, 'transfer_cycle', bind(all_tc))
  })
  qres_sync_conditions(con, names(all_ds))
  unlink(src, recursive = TRUE)
  .msg(sprintf('Done: %d datasets', length(all_ds)))
  invisible(names(all_ds))
}
