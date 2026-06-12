# tamminen_import.R
# Tamminen et al. 2020 PLoS ONE — 8 filamentous marine fungi, 5 carbon sources, 2 instruments
# BioScreen C MBR (OD600, 100-well honeycomb, 30 min intervals, 24°C)
# Cytomat robot (OD595, 96-well, 2–3 h intervals, 24°C)
# Medium: Verduyn minimal + 28 g/L Tropic Marin ASW, pH 4.25
# Carbon sources: 2 g/L each (glucose, xylose, galactose, glycerol); "no C" = control
# DOI: 10.1371/journal.pone.0236822   Zenodo: 10.5281/zenodo.3670890

.tm_msg <- function(...) message("[tamminen] ", ...)

# ── Strain lookup: sheet-name prefix → metadata ───────────────────────────────
.tm_strains <- list(
  PchrysogenumKF657 = list(id="pchr_kf657", species="Penicillium chrysogenum",  strain="KF657", taxon_id="5076"),
  DsalinaLF304      = list(id="dsal_lf304", species="Dendryphiella salina",      strain="LF304", taxon_id=NA_character_),
  HvariaKF560       = list(id="hvar_kf560", species="Halenospora varia",         strain="KF560", taxon_id=NA_character_),
  AcruciatusLF680   = list(id="acru_lf680", species="Asteromyces cruciatus",     strain="LF680", taxon_id=NA_character_),
  LF562             = list(id="lf562",       species="Tritirachium sp.",          strain="LF562", taxon_id=NA_character_),
  LF458             = list(id="lf458",       species="Penicillium pinophilum",    strain="LF458", taxon_id="5083"),
  LF580             = list(id="lf580",       species="Microascus brevicaulis",    strain="LF580", taxon_id=NA_character_),
  LF525             = list(id="lf525",       species="Calcarisporium sp.",        strain="LF525", taxon_id=NA_character_)
)

.tm_strain_key <- function(sheet_name) sub("_(BioScreen|robot)$", "", sheet_name)

# ── Carbon condition → well perturbation metadata ─────────────────────────────
.tm_perturb <- function(cond) {
  if (cond == "no_c") list(type="none", name="",      conc=0,   unit=NA_character_, ctrl=TRUE)
  else                list(type="chemical", name=cond, conc=2.0, unit="g/L",        ctrl=FALSE)
}

.tm_norm_cond <- function(x) {
  x <- tolower(trimws(x %||% ""))
  gsub("[[:space:]]+", "_", x)
}

# ── Column-group finder: returns list(cond_label → integer vector of col indices)
# Works for both 1-row and 2-row header formats.
# cond_row: character vector of the row containing condition labels (NA = separator).
.tm_find_cond_cols <- function(cond_row) {
  groups <- list()
  for (j in seq_along(cond_row)[-1]) {
    lbl <- .tm_norm_cond(cond_row[j])
    if (!nzchar(lbl) || lbl == "na") next
    groups[[lbl]] <- c(groups[[lbl]], j)
  }
  groups
}

# ── BioScreen sheet parser ────────────────────────────────────────────────────
# Header: row 1 = "time (h)" | cond | cond | ... | NA | cond | ...
# Data starts row 2.
.parse_bs_sheet <- function(df, did, plate_id) {
  cond_row  <- as.character(unlist(df[1, ]))
  time_vals <- suppressWarnings(as.numeric(unlist(df[-1, 1])))
  groups    <- .tm_find_cond_cols(cond_row)

  rows <- list()
  for (cond in names(groups)) {
    pm <- .tm_perturb(cond)
    pfx <- gsub("[^a-zA-Z0-9]", "", cond)
    for (ri in seq_along(groups[[cond]])) {
      col_j   <- groups[[cond]][[ri]]
      vals    <- suppressWarnings(as.numeric(unlist(df[-1, col_j])))
      well_nm <- paste0(pfx, "_r", ri)
      rows[[length(rows)+1]] <- list(
        long = data.frame(
          dataset_id=did, plate_id=plate_id,
          well_id=paste0(plate_id, "__", well_nm),
          well=well_nm, time_h=time_vals, signal_type="OD600", value=vals,
          stringsAsFactors=FALSE
        ),
        well_nm=well_nm, cond=cond, pm=pm
      )
    }
  }
  rows
}

# ── Cytomat sheet parser ──────────────────────────────────────────────────────
# Two formats exist:
#   2-row header: row1 = well IDs (P1 S1 …), row2 = conditions, data from row3
#   1-row header: row1 = conditions directly,               data from row2
# Detect by checking if row1 col2 matches a well-ID pattern.
.parse_cy_sheet <- function(df, did, plate_id) {
  r1c2 <- as.character(df[[2]][1]) %||% ""
  two_hdr <- grepl("^P[0-9]+ S[0-9]+$", r1c2)

  if (two_hdr) {
    cond_row  <- as.character(unlist(df[2, ]))
    time_vals <- suppressWarnings(as.numeric(unlist(df[-c(1,2), 1])))
    data_rows <- seq(3, nrow(df))
  } else {
    cond_row  <- as.character(unlist(df[1, ]))
    time_vals <- suppressWarnings(as.numeric(unlist(df[-1, 1])))
    data_rows <- seq(2, nrow(df))
  }
  groups <- .tm_find_cond_cols(cond_row)

  rows <- list()
  for (cond in names(groups)) {
    pm <- .tm_perturb(cond)
    pfx <- gsub("[^a-zA-Z0-9]", "", cond)
    for (ri in seq_along(groups[[cond]])) {
      col_j   <- groups[[cond]][[ri]]
      vals    <- suppressWarnings(as.numeric(unlist(df[data_rows, col_j])))
      well_nm <- paste0(pfx, "_r", ri)
      rows[[length(rows)+1]] <- list(
        long = data.frame(
          dataset_id=did, plate_id=plate_id,
          well_id=paste0(plate_id, "__", well_nm),
          well=well_nm, time_h=time_vals, signal_type="OD600", value=vals,
          stringsAsFactors=FALSE
        ),
        well_nm=well_nm, cond=cond, pm=pm
      )
    }
  }
  rows
}

# ── DB writer (shared for BioScreen and Cytomat) ──────────────────────────────
.tm_write_dataset <- function(con, parsed_rows, long_df, did, plate_id,
                               sm, instrument, wavelength_nm, pq_path, raw_file) {
  arrow::write_parquet(long_df, pq_path)
  .tm_msg(instrument, " ", did, ": ", nrow(long_df), " rows, ",
          length(unique(long_df$well)), " wells")

  DBI::dbExecute(con, "INSERT INTO dataset
    (dataset_id, raw_file, title, doi, year, authors, notes) VALUES (?,?,?,?,?,?,?)", list(
    did, raw_file,
    paste0("Tamminen 2020 ", sm$species, " ", sm$strain, " marine fungi growth (", instrument, ")"),
    "10.1371/journal.pone.0236822",
    2020L,
    "Tamminen A, Happonen P, Barth D, Holmstrom S, Wiebe MG",
    paste0("OD growth curves for marine fungus ", sm$species, " (", sm$strain, ") ",
           "on 5 carbon sources (no C, glucose, xylose, galactose, glycerol; 2 g/L each). ",
           "Medium: Verduyn minimal + 28 g/L Tropic Marin ASW, pH 4.25, 24 C. ",
           "Instrument: ", instrument, ", OD at ", wavelength_nm, " nm. ",
           "Note: inoculum contained ~0.22 g/L glycerol (may affect glycerol-condition interpretation).")
  ))

  DBI::dbExecute(con, "INSERT INTO plate
    (plate_id, dataset_id, plate_format, plate_reader, wavelength_nm, temperature_c)
    VALUES (?,?,?,?,?,?)", list(
    plate_id, did,
    if (grepl("BioScreen", instrument)) "100-well honeycomb" else "96-well",
    instrument,
    wavelength_nm,
    24.0
  ))

  for (r in parsed_rows) {
    pm <- r$pm
    DBI::dbExecute(con, "INSERT INTO well_metadata
      (well_id, plate_id, well, species, taxon_id, strain, medium_type,
       perturbation_type, perturbation, concentration, concentration_unit,
       is_blank, is_control) VALUES (?,?,?,?,?,?,?,?,?,?,?,FALSE,?)", list(
      paste0(plate_id, "__", r$well_nm),
      plate_id, r$well_nm,
      sm$species, sm$taxon_id, sm$strain,
      "Verduyn minimal + ASW",
      pm$type, pm$name, pm$conc, pm$unit,
      pm$ctrl
    ))
  }

  DBI::dbExecute(con, "INSERT INTO raw_measurement_registry
    (dataset_id, storage_type, path, n_rows, n_wells, signal_type, measurement_unit)
    VALUES (?,'parquet',?,?,?,'OD600','OD')",
    list(did, pq_path, nrow(long_df), length(unique(long_df$well))))

  tryCatch(qres_sync_conditions(con, did), error = function(e) NULL)
}

# ── BioScreen import ──────────────────────────────────────────────────────────
tamminen_import_bioscreen <- function(con, source_dir, overwrite = FALSE) {
  f <- file.path(source_dir, "Tamminen_et_al_2020_BioScreen_growth.xlsx")
  if (!file.exists(f)) stop("File not found: ", f)
  pq_dir <- file.path("data", "parquet", "raw_measurements")
  dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)

  sheets <- readxl::excel_sheets(f)
  for (sheet in sheets) {
    key <- .tm_strain_key(sheet)
    sm  <- .tm_strains[[key]]
    if (is.null(sm)) { .tm_msg("Unknown strain key: ", key, " — skipping"); next }

    did      <- paste0("tamminen_2020_", sm$id, "_bs")
    plate_id <- paste0(did, "__plate_1")
    pq_path  <- file.path(pq_dir, paste0(did, ".parquet"))

    if (!overwrite &&
        nrow(DBI::dbGetQuery(con, "SELECT 1 FROM dataset WHERE dataset_id=?", list(did))) > 0L) {
      .tm_msg("Already imported: ", did); next
    }
    for (tbl in c("raw_measurement_registry","well_metadata","plate","dataset"))
      tryCatch(DBI::dbExecute(con, paste0("DELETE FROM ",tbl," WHERE dataset_id=?"), list(did)),
               error=function(e) NULL)
    if (file.exists(pq_path)) file.remove(pq_path)

    df      <- suppressMessages(readxl::read_xlsx(f, sheet=sheet, col_names=FALSE))
    parsed  <- .parse_bs_sheet(df, did, plate_id)
    long_df <- do.call(rbind, lapply(parsed, `[[`, "long"))

    .tm_write_dataset(con, parsed, long_df, did, plate_id, sm,
                      "BioScreen C MBR", 600L, pq_path, basename(f))
  }
  invisible(NULL)
}

# ── Cytomat import ────────────────────────────────────────────────────────────
tamminen_import_cytomat <- function(con, source_dir, overwrite = FALSE) {
  f <- file.path(source_dir, "Tamminen_et_al_2020_Cytomat_growth.xlsx")
  if (!file.exists(f)) stop("File not found: ", f)
  pq_dir <- file.path("data", "parquet", "raw_measurements")
  dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)

  sheets <- readxl::excel_sheets(f)
  for (sheet in sheets) {
    key <- .tm_strain_key(sheet)
    sm  <- .tm_strains[[key]]
    if (is.null(sm)) { .tm_msg("Unknown strain key: ", key, " — skipping"); next }

    did      <- paste0("tamminen_2020_", sm$id, "_cy")
    plate_id <- paste0(did, "__plate_1")
    pq_path  <- file.path(pq_dir, paste0(did, ".parquet"))

    if (!overwrite &&
        nrow(DBI::dbGetQuery(con, "SELECT 1 FROM dataset WHERE dataset_id=?", list(did))) > 0L) {
      .tm_msg("Already imported: ", did); next
    }
    for (tbl in c("raw_measurement_registry","well_metadata","plate","dataset"))
      tryCatch(DBI::dbExecute(con, paste0("DELETE FROM ",tbl," WHERE dataset_id=?"), list(did)),
               error=function(e) NULL)
    if (file.exists(pq_path)) file.remove(pq_path)

    df      <- suppressMessages(readxl::read_xlsx(f, sheet=sheet, col_names=FALSE))
    parsed  <- .parse_cy_sheet(df, did, plate_id)
    long_df <- do.call(rbind, lapply(parsed, `[[`, "long"))

    .tm_write_dataset(con, parsed, long_df, did, plate_id, sm,
                      "Cytomat + DTX 880", 595L, pq_path, basename(f))
  }
  invisible(NULL)
}

# ── Entry point ───────────────────────────────────────────────────────────────
tamminen_import_all <- function(con, source_dir, overwrite = FALSE) {
  qres_migrate_schema(con)
  tamminen_import_bioscreen(con, source_dir, overwrite)
  tamminen_import_cytomat(con, source_dir, overwrite)
  invisible(NULL)
}
