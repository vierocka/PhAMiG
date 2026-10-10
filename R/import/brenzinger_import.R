# R/import/brenzinger_import.R
# ──────────────────────────────────────────────────────────────────────────────
# Import: Brenzinger S, Airoldi M, Ogunleye AJ, Jugovic K, Amstalden MK, Brochado AR
# (2024) Nature Microbiology 9:251–262. "The Vibrio cholerae CBASS phage defence
# system modulates resistance and killing by antifolate antibiotics"
# DOI: 10.1038/s41564-023-01556-y | data: github.com/brochadolab/Brenzinger2024
#
# Conditions (Methods): LB Lennox, 40 µl, 384-well (Greiner 781162), breathable
# membrane, 32 °C, Cytomat 2 incubator shaking at 800 rpm, OD600 on a BioTek
# Synergy H1 every ~40 min. (The screen plotting code labels the axis "OD595nm";
# the Methods and README state OD600.)
#
# Imported parts:
#  1. Compound screen (Data/dCBASS Screen): 94 compounds × 4 concentrations (1:2,
#     rank 1 = highest; µg/ml from Supplementary Table 1) + 8 water wells, one
#     384-well plate per replicate; V. cholerae N16961 WT and ΔCBASS, 3 plates each.
#     Raw OD600 — the authors subtract each well's first reading during analysis.
#  2. Checkerboards (Data/Checkerboards): 96 wells per assay (a quadrant of a
#     384-well plate, relabelled A1–H12 by the authors), 4 replicate quadrants.
#     Horizontal drug: start conc in column 1 → 0 in column 12 (11 equal steps);
#     vertical drug: start conc in row A → 0 in row H (7 equal steps); H12 = no drug
#     (layout from CBs_LoeweFICI_Brenzinger2024.R, get_conc_grad()).
#     Deposited values are already background-subtracted ("_cleaned" plates).
# Not imported: Data/MICs/Parsed_MIC_data.txt (background-subtracted extracts of
# plates not in the repository; strain codes need curation).
#
# Usage:
#   source('R/import/brenzinger_import.R')
#   brenzinger_import_all(con, source_dir = '/path/to/Brenzinger_2024_Vcholerae_CBASS')
#   source_dir must contain Brenzinger2024-main/ and supplementary/MOESM2.xlsx
# ──────────────────────────────────────────────────────────────────────────────

BZ_DOI        <- '10.1038/s41564-023-01556-y'
BZ_YEAR       <- 2024L
BZ_AUTHORS    <- 'Brenzinger S, Airoldi M, Ogunleye AJ, Jugovic K, Amstalden MK, Brochado AR'
BZ_LICENSE    <- 'GPL-3.0 (GitHub repository)'
BZ_SOURCE_URL <- 'https://github.com/brochadolab/Brenzinger2024'
BZ_MEDIUM     <- 'LB Lennox'
BZ_DRUG_NAMES <- c(SMX = 'Sulfamethoxazole', TMP = 'Trimethoprim', Ara = 'L-arabinose')

# Strain labels (Supplementary Table 2)
BZ_STRAINS <- list(
  wt     = list(species = 'Vibrio cholerae', taxon = '666', strain = 'N16961 (AB10018) WT'),
  dCBASS = list(species = 'Vibrio cholerae', taxon = '666', strain = 'N16961 ΔCBASS ΔVC_0178-0181 (AB30122)'),
  pBAD_CBASS_VC    = list(species = 'E. coli', taxon = '562', strain = 'BW27783 pBAD18-VC_CBASS (AB30364)'),
  pBAD_CBASS_3234a = list(species = 'E. coli', taxon = '562', strain = 'BW27783 pBAD18-3234_CBASS (AB30373)'),
  pBAD_CBASS_U541  = list(species = 'E. coli', taxon = '562', strain = 'BW27783 pBAD18-U5/41_CBASS (AB30374)'),
  pBAD_CBASS_TW1   = list(species = 'E. coli', taxon = '562', strain = 'BW27783 pBAD18-TW_CBASS_I (AB30375)'),
  pBAD_CBASS_TW2   = list(species = 'E. coli', taxon = '562', strain = 'BW27783 pBAD18-TW_CBASS_II (AB30376)'),
  pBAD_sfGFP       = list(species = 'E. coli', taxon = '562', strain = 'BW27783 pBAD18-sfGFP (AB30387)'))

.bz_conc_grad <- function(c, steps) {          # authors' get_conc_grad(): 0 … c ascending
  step <- c / steps
  v <- c - ((steps + 1):1) * step
  round(c(v[2:(steps + 1)], c), 5)
}

.bz_tbl <- function(path) {
  # comment.char = '' — colour codes like "#9F2B4B" in the plate tables;
  # fill = TRUE — Database.txt ends with an incomplete 'P07' line
  utils::read.table(path, header = TRUE, sep = '\t', quote = '', strip.white = TRUE,
                    comment.char = '', fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
}

# Wide plate table (Time + wells) → long data.frame for one plate
.bz_long <- function(wide, did, pid) {
  wells <- setdiff(names(wide), 'Time')
  data.frame(dataset_id = did, plate_id = pid,
             well_id = paste0(pid, '__', rep(wells, each = nrow(wide))),
             well = rep(wells, each = nrow(wide)), time_h = rep(wide$Time, length(wells)),
             signal_type = 'OD600', value = as.numeric(unlist(wide[wells], use.names = FALSE)),
             stringsAsFactors = FALSE)
}

brenzinger_import_all <- function(
    con,
    source_dir  = NULL,
    parquet_dir = 'data/parquet/raw_measurements',
    overwrite   = FALSE,
    verbose     = TRUE
) {
  if (is.null(source_dir)) stop('source_dir is required')
  requireNamespace('DBI'); requireNamespace('arrow'); requireNamespace('readxl')
  requireNamespace('data.table')
  .msg <- function(...) if (verbose) message('[brenzinger] ', ...)
  for (sql in c("ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS dataset_id TEXT",
                "ALTER TABLE well_metadata ADD COLUMN IF NOT EXISTS gene_category TEXT"))
    tryCatch(DBI::dbExecute(con, sql), error = function(e) NULL)
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)

  n_exist <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM dataset WHERE dataset_id LIKE 'brenzinger_2024_%'")$n
  if (n_exist > 0L && !overwrite) {
    .msg(n_exist, ' datasets already present — pass overwrite = TRUE to re-import')
    return(invisible(NULL))
  }

  repo <- file.path(source_dir, 'Brenzinger2024-main')
  all_ds <- list(); all_pl <- list(); all_wm <- list(); all_reg <- list()
  add_dataset <- function(did, title, raw_file, notes, long, wm, plate_ids, plate_format) {
    pq_path <- file.path(parquet_dir, paste0(did, '.parquet'))
    arrow::write_parquet(long, pq_path)
    all_ds[[did]] <<- data.frame(
      dataset_id = did, raw_file = raw_file, title = title, doi = BZ_DOI, year = BZ_YEAR,
      authors = BZ_AUTHORS, license = BZ_LICENSE, source_url = BZ_SOURCE_URL,
      time_unit = 'hours', notes = notes, stringsAsFactors = FALSE)
    all_pl[[did]] <<- data.frame(
      plate_id = plate_ids, dataset_id = did, plate_format = plate_format,
      plate_reader = 'Synergy H1 (BioTek)', wavelength_nm = 600, temperature_c = 32,
      shake_freq_rpm = 800, stringsAsFactors = FALSE)
    all_wm[[did]] <<- wm
    all_reg[[did]] <<- data.frame(
      dataset_id = did, storage_type = 'parquet', path = pq_path, n_rows = nrow(long),
      n_wells = nrow(wm), signal_type = 'OD600', measurement_unit = 'OD', stringsAsFactors = FALSE)
  }
  wm_frame <- function(did, pid, well, st, pert_type, pert, conc, unit, is_control, replicate)
    data.frame(well_id = paste0(pid, '__', well), plate_id = pid, well = well,
               species = st$species, taxon_id = st$taxon, strain = st$strain,
               medium_state = 'liquid', medium_type = BZ_MEDIUM,
               perturbation_type = pert_type, perturbation = pert, concentration = conc,
               concentration_unit = unit, is_blank = FALSE, is_control = is_control,
               replicate = replicate, dataset_id = did, gene_category = NA_character_,
               stringsAsFactors = FALSE)

  # ── 1. Compound screen ──────────────────────────────────────────────────────
  sdir <- file.path(repo, 'Code', 'dCBASS Screen')
  map  <- .bz_tbl(file.path(sdir, 'Map.txt'))
  map$Drug[map$Drug == 'Sulfamethoxazol'] <- 'Sulfamethoxazole'   # spelling in Map.txt
  s1 <- suppressMessages(as.data.frame(readxl::read_xlsx(
    file.path(source_dir, 'supplementary', 'MOESM2.xlsx'), sheet = 'SuppTab1')))[-1, ]
  s1 <- s1[!is.na(s1$Drug), ]
  conc_tab <- as.matrix(sapply(s1[, 4:7], as.numeric)); rownames(conc_tab) <- s1$Drug
  is_water <- map$Drug == 'Water'
  miss <- setdiff(map$Drug[!is_water], rownames(conc_tab))
  if (length(miss)) stop('Screen compounds without concentration: ', paste(miss, collapse = ', '))
  map$conc <- NA_real_
  map$conc[!is_water] <- conc_tab[cbind(match(map$Drug[!is_water], rownames(conc_tab)),
                                        map$ConcMock[!is_water])]
  water_wells <- paste(map$Well[is_water], collapse = ', ')

  db <- .bz_tbl(file.path(sdir, 'Database.txt'))
  db <- db[nzchar(db$file_id %||% '') & !is.na(db$file_id), ]
  for (bug in c('wt', 'dCBASS')) {
    rows <- db[sub('_[0-9]+$', '', db$Strain) == bug, ]
    did  <- paste0('brenzinger_2024_screen_', tolower(bug))
    long <- list(); wm <- list()
    for (i in seq_len(nrow(rows))) {
      rep_n <- rows$Rep_Nr[i]; pid <- paste0(did, '__plate_', rep_n)
      wide <- .bz_tbl(file.path(repo, 'Data', 'dCBASS Screen', rows$file_id[i]))
      if (!setequal(setdiff(names(wide), 'Time'), map$Well)) stop('Unexpected wells in ', rows$file_id[i])
      long[[i]] <- .bz_long(wide, did, pid)
      mm <- map[match(setdiff(names(wide), 'Time'), map$Well), ]
      water <- mm$Drug == 'Water'
      wm[[i]] <- wm_frame(did, pid, mm$Well, BZ_STRAINS[[bug]],
                          ifelse(water, 'none', 'chemical'), ifelse(water, NA, mm$Drug),
                          ifelse(water, NA, mm$conc), ifelse(water, NA, 'µg/ml'), water, rep_n)
    }
    add_dataset(did,
      sprintf('V. cholerae N16961 %s compound screen, 94 compounds × 4 concentrations (Brenzinger 2024)',
              if (bug == 'wt') 'WT' else 'ΔCBASS'),
      paste(rows$file_id, collapse = '; '),
      paste0('Raw OD600 as deposited (the authors subtract each well\'s first reading for analysis). ',
             '384-well plates, one plate per biological replicate (', nrow(rows), '). Compound ',
             'concentrations from Supplementary Table 1 (rank 1 = highest, 1:2 dilutions); ',
             'Water wells (no-compound controls): ', water_wells, '. ',
             'Inoculum OD600 ~0.0003; data beyond 10 h were not used by the authors ',
             '(occasional late growth of resistant mutants).'),
      data.table::rbindlist(long), data.table::rbindlist(wm),
      paste0(did, '__plate_', rows$Rep_Nr), '384')
  }
  .msg('Screen: ', paste(names(all_ds), collapse = ', '))

  # ── 2. Checkerboards ────────────────────────────────────────────────────────
  cdir <- file.path(repo, 'Code', 'Checkerboards')
  cb_db <- rbind(
    transform(.bz_tbl(file.path(cdir, 'PlateDatabase_combination.txt')),
              bug = bug)[, c('CB_id', 'Replicate', 'bug', 'Horiz_drug', 'Vert_drug', 'Horiz_startC', 'Vert_startC')],
    transform(.bz_tbl(file.path(cdir, 'PlateDatabase_ExpressionSystems.txt')),
              bug = sub('_[0-9]+$', '', CB_id))[, c('CB_id', 'Replicate', 'bug', 'Horiz_drug', 'Vert_drug', 'Horiz_startC', 'Vert_startC')])
  for (bug in unique(cb_db$bug)) {
    rows <- cb_db[cb_db$bug == bug, ]
    st   <- BZ_STRAINS[[bug]]; if (is.null(st)) stop('No strain label for checkerboard ', bug)
    did  <- paste0('brenzinger_2024_cb_', tolower(gsub('[^A-Za-z0-9]+', '_', bug)))
    long <- list(); wm <- list()
    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]; pid <- paste0(did, '__plate_', r$Replicate)
      wide <- .bz_tbl(file.path(repo, 'Data', 'Checkerboards', paste0(r$CB_id, '.txt')))
      wells <- setdiff(names(wide), 'Time')
      if (length(wells) != 96L) stop('Expected 96 wells in ', r$CB_id)
      long[[i]] <- .bz_long(wide, did, pid)
      row_i <- match(substr(wells, 1, 1), LETTERS[1:8]); col_j <- as.integer(substring(wells, 2))
      ch <- .bz_conc_grad(as.numeric(r$Horiz_startC), 11)[13L - col_j]
      cv <- .bz_conc_grad(as.numeric(r$Vert_startC), 7)[9L - row_i]
      hn <- BZ_DRUG_NAMES[[r$Horiz_drug]]; vn <- BZ_DRUG_NAMES[[r$Vert_drug]]
      v_unit <- if (r$Vert_drug == 'Ara') '' else ' µg/ml'
      pert <- ifelse(ch > 0 & cv > 0, sprintf('%s %s µg/ml + %s %s%s', hn, signif(ch, 4), vn, signif(cv, 4), v_unit),
              ifelse(ch > 0, hn, ifelse(cv > 0, vn, NA)))
      conc <- ifelse(ch > 0 & cv > 0, NA, ifelse(ch > 0, ch, ifelse(cv > 0, cv, NA)))
      unit <- ifelse(ch > 0 & cv == 0, 'µg/ml', ifelse(cv > 0 & ch == 0 & r$Vert_drug != 'Ara', 'µg/ml', NA))
      wm[[i]] <- wm_frame(did, pid, wells, st, ifelse(ch == 0 & cv == 0, 'none', 'chemical'),
                          pert, conc, unit, ch == 0 & cv == 0, r$Replicate)
    }
    r <- rows[1, ]
    add_dataset(did,
      sprintf('%s %s checkerboard %s × %s (Brenzinger 2024)', st$species, st$strain,
              BZ_DRUG_NAMES[[r$Horiz_drug]], BZ_DRUG_NAMES[[r$Vert_drug]]),
      paste(paste0(rows$CB_id, '.txt'), collapse = '; '),
      paste0('OD600 as deposited — already background-subtracted by the authors ("cleaned" plates). ',
             nrow(rows), ' replicate assays, each a 96-well quadrant of a 384-well plate relabelled A1–H12. ',
             sprintf('%s 0–%s µg/ml across columns 12→1 (11 equal steps); %s 0–%s%s across rows H→A (7 equal steps); H12 = no drug. ',
                     BZ_DRUG_NAMES[[r$Horiz_drug]], r$Horiz_startC, BZ_DRUG_NAMES[[r$Vert_drug]], r$Vert_startC,
                     if (r$Vert_drug == 'Ara') '' else ' µg/ml'),
             if (r$Vert_drug == 'Ara') 'Arabinose (pBAD inducer) unit is not given in the authors\' plate table (start 0.5); the Methods state "5 µg/ml". ' else '',
             'The authors replace the 2nd time point by the mean of points 1 and 3 (membrane bulging); kept as recorded here.'),
      data.table::rbindlist(long), data.table::rbindlist(wm),
      paste0(did, '__plate_', rows$Replicate), '384 (96-well quadrant)')
  }

  bind <- function(x) data.table::rbindlist(x, use.names = TRUE)
  .msg(sprintf('Writing %d datasets, %d curves to DB...', length(all_ds), nrow(bind(all_wm))))
  DBI::dbWithTransaction(con, {
    if (n_exist > 0L) for (sql in c(
      "DELETE FROM well_metadata WHERE plate_id LIKE 'brenzinger\\_2024\\_%' ESCAPE '\\'",
      "DELETE FROM raw_measurement_registry WHERE dataset_id LIKE 'brenzinger\\_2024\\_%' ESCAPE '\\'",
      "DELETE FROM dataset_conditions WHERE dataset_id LIKE 'brenzinger\\_2024\\_%' ESCAPE '\\'",
      "DELETE FROM plate WHERE dataset_id LIKE 'brenzinger\\_2024\\_%' ESCAPE '\\'",
      "DELETE FROM dataset WHERE dataset_id LIKE 'brenzinger\\_2024\\_%' ESCAPE '\\'"
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
