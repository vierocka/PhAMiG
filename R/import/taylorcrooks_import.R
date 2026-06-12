# taylorcrooks_import.R
# TaylorCrooks 2021  — Porphyromonas somerae KFU
# Gentamicin time-series OD600 growth curves (3 replicates × 4 doses)
# Source: TaylorCrooks_2021_Porphyromonas_somerae_kfu/Gentamicin growth curve.txt
# DOI: 10.3389/fmicb.2021.674835

.tc_msg <- function(...) message("[taylorcrooks] ", ...)

taylorcrooks_import_gentamicin <- function(con, source_dir, overwrite = FALSE) {
  f <- file.path(source_dir, "Gentamicin growth curve.txt")
  if (!file.exists(f)) stop("File not found: ", f)

  did      <- "tcrooks_2021_psom_kfu_gent"
  plate_id <- paste0(did, "__plate_1")
  pq_dir   <- file.path("data", "parquet", "raw_measurements")
  dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)
  pq_path  <- file.path(pq_dir, paste0(did, ".parquet"))

  if (!overwrite &&
      nrow(DBI::dbGetQuery(con, "SELECT 1 FROM dataset WHERE dataset_id = ?", list(did))) > 0L) {
    .tc_msg("Already imported: ", did, " (use overwrite=TRUE to re-import)")
    return(invisible(NULL))
  }

  # Read — ISO-8859-1 (µ = 0xB5), CRLF terminators
  raw <- read.table(f, header = TRUE, sep = "\t", check.names = FALSE,
                    fileEncoding = "latin1", stringsAsFactors = FALSE)

  # Extract condition columns BEFORE adding replicate column
  cond_cols <- colnames(raw)[-1]   # all cols except "Time"

  # 3 replicates per time point — assign replicate index within each time group
  raw <- raw[order(raw[[1]]), ]
  reps_per_time <- as.integer(table(raw[[1]]))
  stopifnot(all(reps_per_time == 3))
  raw$replicate <- unlist(lapply(reps_per_time, seq_len))

  # Well name from condition column name
  .well_name <- function(col) {
    if (grepl("No_Antibiotic", col, ignore.case = TRUE)) return("ctrl")
    m <- regmatches(col, regexpr("^[0-9]+", col))
    paste0("gent_", if (length(m) > 0) m else "?", "ugmL")
  }

  # Numeric concentration from column name (0 for no-antibiotic)
  .conc_val <- function(col) {
    if (grepl("No_Antibiotic", col, ignore.case = TRUE)) return(0)
    m <- regmatches(col, regexpr("^[0-9]+", col))
    if (length(m) > 0) as.numeric(m) else NA_real_
  }

  # Build long data frame — well_id = plate_id__well (matches baker_import convention)
  rows <- lapply(cond_cols, function(cc) {
    lapply(1:3, function(ri) {
      sub_df  <- raw[raw$replicate == ri, ]
      well_nm <- paste0(.well_name(cc), "_r", ri)
      data.frame(
        dataset_id  = did,
        plate_id    = plate_id,
        well_id     = paste0(plate_id, "__", well_nm),
        well        = well_nm,
        time_h      = sub_df[[1]],
        signal_type = "OD600",
        value       = sub_df[[cc]],
        stringsAsFactors = FALSE
      )
    })
  })
  long_df <- do.call(rbind, do.call(c, rows))

  # Clear old records
  if (overwrite) {
    for (tbl in c("raw_measurement_registry", "well_metadata", "plate", "dataset"))
      tryCatch(DBI::dbExecute(con, paste0("DELETE FROM ", tbl, " WHERE dataset_id = ?"), list(did)),
               error = function(e) NULL)
    if (file.exists(pq_path)) file.remove(pq_path)
  }

  # Write Parquet
  arrow::write_parquet(long_df, pq_path)
  .tc_msg("Parquet: ", pq_path, " (", nrow(long_df), " rows, ", length(unique(long_df$well)), " wells)")

  # dataset
  DBI::dbExecute(con, "INSERT INTO dataset
    (dataset_id, raw_file, title, doi, year, authors, notes)
    VALUES (?,?,?,?,?,?,?)", list(
    did,
    basename(f),
    "Taylor & Crooks 2021 Porphyromonas somerae KFU gentamicin growth curves",
    "10.3389/fmicb.2021.674835",
    2021L,
    "Taylor AM, Crooks WL",
    paste0("OD600 growth curves for P. somerae strain KFU under gentamicin ",
           "(0, 1, 10, 100 µg/mL), anaerobic culture, 37°C. ",
           "3 biological replicates per condition. 12 time points (2-24 h).")
  ))

  # plate
  DBI::dbExecute(con, "INSERT INTO plate (plate_id, dataset_id, plate_format, temperature_c)
    VALUES (?, ?, 'tube', 37.0)", list(plate_id, did))

  # well_metadata — one row per (condition × replicate)
  for (cc in cond_cols) {
    conc    <- .conc_val(cc)
    is_ctrl <- conc == 0
    for (ri in 1:3) {
      well_nm <- paste0(.well_name(cc), "_r", ri)
      DBI::dbExecute(con, "INSERT INTO well_metadata
        (well_id, plate_id, well, species, taxon_id, strain,
         medium_type, perturbation_type, perturbation,
         concentration, concentration_unit, is_blank, is_control)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,FALSE,?)", list(
        paste0(plate_id, "__", well_nm),
        plate_id, well_nm,
        "Porphyromonas somerae", "235623", "KFU",
        "blood broth",
        if (is_ctrl) "none" else "chemical",
        if (is_ctrl) "" else "Gentamicin",
        conc,
        if (is_ctrl) NA_character_ else "µg/mL",
        is_ctrl
      ))
    }
  }

  # raw_measurement_registry
  DBI::dbExecute(con, "INSERT INTO raw_measurement_registry
    (dataset_id, storage_type, path, n_rows, n_wells, signal_type, measurement_unit)
    VALUES (?, 'parquet', ?, ?, ?, 'OD600', 'OD')",
    list(did, pq_path, nrow(long_df), length(unique(long_df$well))))

  tryCatch(qres_sync_conditions(con, did), error = function(e) NULL)
  .tc_msg("Done: ", did)
  invisible(NULL)
}

taylorcrooks_import_all <- function(con, source_dir, overwrite = FALSE) {
  qres_migrate_schema(con)
  taylorcrooks_import_gentamicin(con, source_dir, overwrite)
  invisible(NULL)
}
