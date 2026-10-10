# eisner_import.R
# Eisner et al. 2022 Front. Microbiol. — Myxococcus xanthus rpoB growth fitness
# 4 strains (GJV1 wt, YTY2 rpoB-S534L, YTY25 rpoB-S534L, YTY26 + variants)
# 3 casitone concentrations (0.2%, 0.5%, 1% w/v) as nutrient level perturbation
# (stored as perturbation_type 'chemical', the controlled vocabulary for compound levels)
# 4 biological replicate plates, 48-well (TECAN Infinite M200 PRO), OD595, 32°C
# DOI: 10.3389/fmicb.2022.817080
# Source: Figure_3.xlsx, sheet "raw values", rows 1-303 (raw section only)

.ei_msg <- function(...) message("[eisner] ", ...)

# ── Condition label parser ────────────────────────────────────────────────────
# Returns list(strain, conc, is_blank) or NULL to skip the well.
.ei_parse_cond <- function(lbl) {
  lbl <- trimws(lbl %||% "")
  if (!nzchar(lbl) || lbl == "NA") return(NULL)

  # Skip wells with incomplete or unrecognised labels
  if (grepl("^GJV1 %$", lbl))    return(NULL)   # missing concentration
  if (grepl("^18-2 ",   lbl))    return(NULL)   # unknown strain

  # Medium blank (uninoculated CTT)
  if (grepl("^control ", lbl)) {
    conc_str <- sub("^control ([0-9.]+)%$", "\\1", lbl)
    conc <- suppressWarnings(as.numeric(conc_str))
    if (is.na(conc)) return(NULL)
    return(list(strain = NA_character_, conc = conc, is_blank = TRUE))
  }

  # Strain wells: "STRAIN CONC%"
  # Normalise typo "YYT25" and prime variant "YTY25'" to "YTY25"
  m <- regmatches(lbl, regexec("^([A-Za-z0-9' ]+?) ([0-9.]+)%$", lbl))[[1]]
  if (length(m) < 3) return(NULL)
  strain_raw <- trimws(m[2])
  conc       <- suppressWarnings(as.numeric(m[3]))
  if (is.na(conc)) return(NULL)

  strain <- switch(strain_raw,
    "YTY25'" = "YTY25",
    "YYT25"  = "YTY25",
    strain_raw
  )
  list(strain = strain, conc = conc, is_blank = FALSE)
}

# ── Per-block parser (one biological replicate plate) ────────────────────────
# rv: full raw values data frame (no column names)
# b_start: 1-based column index where this block begins (1, 53, 105, 157)
# Returns list(long_df, meta_list)
.ei_parse_block <- function(rv, b_start, did, plate_id) {
  # Headers: row2 = well positions (A1, A2, …), row3 = condition labels
  # Meta cols in block: b_start (Cycle Nr.), b_start+1 (Time [s]), b_start+2 (Temp [°C])
  # Data cols: b_start+3 … b_start+50  (48 wells)
  well_pos_row <- as.character(unlist(rv[2, (b_start + 3):(b_start + 50)]))
  cond_row     <- as.character(unlist(rv[3, (b_start + 3):(b_start + 50)]))

  # Time vector from block 1 col (b_start+1), data rows 4-303
  time_s <- suppressWarnings(as.numeric(unlist(rv[4:303, b_start + 1])))
  time_h <- time_s / 3600

  long_rows <- list()
  meta_list <- list()

  for (j in seq_along(cond_row)) {
    cond_lbl <- cond_row[j]
    well_pos <- well_pos_row[j]
    parsed   <- .ei_parse_cond(cond_lbl)
    if (is.null(parsed)) next
    if (!nzchar(well_pos %||% "") || well_pos == "NA") next

    col_idx <- b_start + 3 + j - 1   # absolute 1-based column
    vals    <- suppressWarnings(as.numeric(unlist(rv[4:303, col_idx])))
    well_id <- paste0(plate_id, "__", well_pos)

    long_rows[[length(long_rows) + 1]] <- data.frame(
      dataset_id  = did,
      plate_id    = plate_id,
      well_id     = well_id,
      well        = well_pos,
      time_h      = time_h,
      signal_type = "OD600",
      value       = vals,
      stringsAsFactors = FALSE
    )

    meta_list[[length(meta_list) + 1]] <- list(
      well_id  = well_id,
      plate_id = plate_id,
      well     = well_pos,
      strain   = parsed$strain,
      conc     = parsed$conc,
      is_blank = parsed$is_blank
    )
  }

  list(
    long_df   = do.call(rbind, long_rows),
    meta_list = meta_list
  )
}

# ── Main import function ──────────────────────────────────────────────────────
eisner_import_growth <- function(con, source_dir, overwrite = FALSE) {
  f <- file.path(source_dir, "Figure_3.xlsx")
  if (!file.exists(f)) stop("File not found: ", f)

  did    <- "eisner_2022_mx_rpob"
  pq_dir <- file.path("data", "parquet", "raw_measurements")
  dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)

  # Idempotency check (dataset-level)
  already <- nrow(DBI::dbGetQuery(con,
    "SELECT 1 FROM dataset WHERE dataset_id = ?", list(did))) > 0L
  if (already && !overwrite) {
    .ei_msg("Already imported: ", did, " (use overwrite=TRUE to re-import)")
    return(invisible(NULL))
  }

  .ei_msg("Reading ", basename(f), " ...")
  rv <- suppressMessages(readxl::read_xlsx(f, sheet = "raw values", col_names = FALSE))

  # Clear existing records for all 4 plates
  if (overwrite || already) {
    # well_metadata has no dataset_id column — must delete by plate_id
    for (b_del in 1:4)
      tryCatch(DBI::dbExecute(con, "DELETE FROM well_metadata WHERE plate_id = ?",
        list(paste0(did, "__plate_", b_del))), error = function(e) NULL)
    for (tbl in c("raw_measurement_registry", "plate", "dataset"))
      tryCatch(DBI::dbExecute(con,
        paste0("DELETE FROM ", tbl, " WHERE dataset_id = ?"), list(did)),
        error = function(e) NULL)
    for (b_del in 1:4) {
      pq <- file.path(pq_dir, paste0(did, "_plate", b_del, ".parquet"))
      if (file.exists(pq)) file.remove(pq)
    }
  }

  # Insert dataset record
  DBI::dbExecute(con, "INSERT INTO dataset
    (dataset_id, raw_file, title, doi, year, authors, notes)
    VALUES (?,?,?,?,?,?,?)", list(
    did,
    basename(f),
    "Eisner 2022 Myxococcus xanthus rpoB growth at varying casitone",
    "10.3389/fmicb.2022.817080",
    2022L,
    "Eisner SA, Velicer GJ, Yu YTN",
    paste0("OD595 vegetative growth of M. xanthus strains (GJV1 wt, YTY2 rpoB-S534L, ",
           "YTY25 rpoB-S534L+Mxan6547wt) at 0.2%, 0.5%, 1% casitone (CTT medium). ",
           "4 biological replicate plates, 48-well, TECAN Infinite M200 PRO, 32 C, 31.4 h. ",
           "Study: rpoB S534L mutation shifts nutrient threshold for fruiting body development.")
  ))

  # Block column starts (1-based)
  block_starts <- c(1L, 53L, 105L, 157L)
  total_rows_written <- 0L

  for (b in seq_along(block_starts)) {
    plate_id <- paste0(did, "__plate_", b)
    pq_path  <- file.path(pq_dir, paste0(did, "_plate", b, ".parquet"))

    result <- .ei_parse_block(rv, block_starts[b], did, plate_id)
    long_df   <- result$long_df
    meta_list <- result$meta_list

    if (is.null(long_df) || nrow(long_df) == 0L) {
      .ei_msg("Block ", b, ": no data parsed, skipping")
      next
    }

    arrow::write_parquet(long_df, pq_path)
    n_wells <- length(unique(long_df$well))
    .ei_msg("Plate ", b, ": ", nrow(long_df), " rows, ", n_wells, " wells -> ", pq_path)

    DBI::dbExecute(con, "INSERT INTO plate
      (plate_id, dataset_id, plate_format, plate_reader, wavelength_nm, temperature_c)
      VALUES (?,?,'48-well','TECAN Infinite M200 PRO',595,32.0)", list(plate_id, did))

    for (m in meta_list) {
      if (m$is_blank) {
        DBI::dbExecute(con, "INSERT INTO well_metadata
          (well_id, plate_id, well, species, taxon_id, strain,
           medium_type, perturbation_type, perturbation,
           concentration, concentration_unit, is_blank, is_control)
          VALUES (?,?,?,'','','','CTT','none','',?,NULL,TRUE,FALSE)", list(
          m$well_id, m$plate_id, m$well, m$conc
        ))
      } else {
        DBI::dbExecute(con, "INSERT INTO well_metadata
          (well_id, plate_id, well, species, taxon_id, strain,
           medium_type, perturbation_type, perturbation,
           concentration, concentration_unit, is_blank, is_control)
          VALUES (?,?,?,'Myxococcus xanthus','34',?,'CTT','chemical','casitone',?,'%(w/v)',FALSE,FALSE)", list(
          m$well_id, m$plate_id, m$well, m$strain, m$conc
        ))
      }
    }

    total_rows_written <- total_rows_written + nrow(long_df)
  }

  # Registry: one row per dataset (PRIMARY KEY = dataset_id), aggregate across plates
  pq_paths_all <- file.path(pq_dir, paste0(did, "_plate", 1:4, ".parquet"))
  pq_paths_all <- pq_paths_all[file.exists(pq_paths_all)]
  DBI::dbExecute(con, "INSERT INTO raw_measurement_registry
    (dataset_id, storage_type, path, n_rows, n_wells, signal_type, measurement_unit)
    VALUES (?, 'parquet', ?, ?, ?, 'OD600', 'OD')",
    list(did,
         paste(pq_paths_all, collapse = ";"),
         total_rows_written,
         as.integer(total_rows_written / 300L)))  # 300 time points per well

  tryCatch(qres_sync_conditions(con, did), error = function(e) NULL)
  .ei_msg("Done: ", did, " (", total_rows_written, " total Parquet rows)")
  invisible(NULL)
}

# ── Entry point ───────────────────────────────────────────────────────────────
eisner_import_all <- function(con, source_dir, overwrite = FALSE) {
  qres_migrate_schema(con)
  eisner_import_growth(con, source_dir, overwrite)
  invisible(NULL)
}
