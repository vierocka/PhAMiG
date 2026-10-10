# Waithaka et al. (2022) "RBSR regulates mRNA processing in T. brucei"
# PLoS Neglected Tropical Diseases  DOI: 10.1371/journal.pntd.0010876
#
# Two data types in the companion folder:
#   1. RBSR RNAi growth curves     → signal_type = 'cell_count', standard time-series Parquet
#   2. Puromycin dose–response     → signal_type = 'fluorescence', dose_response_registry Parquet
#
# Usage:
#   source('R/db/connect.R'); source('R/import/waithaka_import.R')
#   con <- qres_connect('data/qres.duckdb')
#   waithaka_import_all(con, '/path/to/Waithaka_2022_Trypanosoma_brucei/')
#   con$disconnect()

.wmsg <- function(...) message(paste0('[waithaka] ', ...))

# ─── RBSR growth curves ───────────────────────────────────────────────────────

# Parse one sheet from "RBSR RNAi growth curves.xlsx".
# Returns data.frame: condition, time_h, value (cumulative cell count).
.parse_rbsr_sheet <- function(df) {
  df <- as.data.frame(df[-1, ])   # drop header row

  # Find the condition-name column on the right side of the sheet.
  # Conditions look like "Parent:", "c2 -tet:", "c2 +tet:", etc.
  cond_col <- NA_integer_
  for (cc in seq(min(8L, ncol(df)), min(11L, ncol(df)))) {
    vals     <- trimws(as.character(df[[cc]]))
    non_na   <- vals[vals != "" & vals != "NA"]
    if (length(non_na) > 0 &&
        any(grepl("parent|\\bc[0-9]\\b|\\+tet|-tet", non_na, ignore.case = TRUE))) {
      cond_col <- cc; break
    }
  }

  cond_labels <- if (!is.na(cond_col)) {
    raw_labels <- trimws(as.character(df[[cond_col]]))
    trimws(gsub(":$", "", raw_labels[raw_labels != "" & raw_labels != "NA"]))
  } else {
    character(0)
  }

  # Block starts: rows where col 1 is non-NA (cell line ID)
  block_starts <- which(!is.na(df[[1]]) & trimws(as.character(df[[1]])) != "")
  if (length(block_starts) == 0) return(NULL)

  if (length(cond_labels) < length(block_starts))
    cond_labels <- c(cond_labels, paste0("condition_", seq_len(length(block_starts) - length(cond_labels))))

  result <- vector("list", length(block_starts))
  for (i in seq_along(block_starts)) {
    start_row <- block_starts[i]
    end_row   <- if (i < length(block_starts)) block_starts[i + 1] - 1L else nrow(df)
    block     <- df[start_row:end_row, ]

    time_h <- suppressWarnings(as.numeric(as.character(block[[2]])))
    value  <- suppressWarnings(as.numeric(as.character(block[[6]])))   # cumulative count

    valid  <- !is.na(time_h) & !is.na(value) & value > 0
    if (!any(valid)) next

    result[[i]] <- data.frame(
      condition = cond_labels[[i]],
      time_h    = time_h[valid],
      value     = value[valid],
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, Filter(Negate(is.null), result))
}

.cond_to_well_name <- function(cond) {
  gsub("[^a-zA-Z0-9_-]", "_", trimws(gsub("\\s+", "_", cond)))
}

waithaka_import_rbsr <- function(con, source_dir, overwrite = FALSE) {
  f <- file.path(source_dir, "RBSR RNAi growth curves.xlsx")
  if (!file.exists(f)) stop("Not found: ", f)
  sheets <- readxl::excel_sheets(f)
  .wmsg("RBSR sheets: ", paste(sheets, collapse = ", "))

  pq_dir <- "data/parquet/raw_measurements"
  dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)

  for (sheet_name in sheets) {
    sheet_tag <- tolower(gsub("[^a-zA-Z0-9]", "", sheet_name))
    did       <- paste0("waithaka_2022_tbr_", sheet_tag)

    if (!overwrite) {
      exists <- tryCatch(
        nrow(DBI::dbGetQuery(con, "SELECT 1 FROM dataset WHERE dataset_id = ?", list(did))) > 0L,
        error = function(e) FALSE)
      if (exists) { .wmsg("Skip (exists): ", did); next }
    }

    .wmsg("Parsing: ", sheet_name)
    parsed <- .parse_rbsr_sheet(readxl::read_xlsx(f, sheet = sheet_name, col_names = FALSE))
    if (is.null(parsed) || nrow(parsed) == 0L) {
      .wmsg("Warning: no data in ", sheet_name); next
    }

    conditions      <- unique(parsed$condition)
    well_name_map   <- setNames(.cond_to_well_name(conditions), conditions)
    well_id_map     <- setNames(paste0(did, "_", seq_along(conditions)), conditions)
    parsed$well_id  <- well_id_map[parsed$condition]
    parsed$well     <- well_name_map[parsed$condition]

    # Write Parquet — standard schema: dataset_id, plate_id, well_id, well, time_h, signal_type, value
    plate_id <- paste0(did, "_p1")
    pq_path  <- file.path(pq_dir, paste0(did, ".parquet"))
    if (overwrite && file.exists(pq_path)) file.remove(pq_path)
    pq_df <- parsed[, c("well_id", "well", "time_h", "value")]
    pq_df$dataset_id  <- did
    pq_df$plate_id    <- plate_id
    pq_df$signal_type <- "cell_count"
    pq_df <- pq_df[, c("dataset_id", "plate_id", "well_id", "well", "time_h", "signal_type", "value")]
    arrow::write_parquet(pq_df, pq_path)
    .wmsg("Parquet: ", pq_path, " (", nrow(pq_df), " rows, ", length(conditions), " wells)")

    # Clear old records
    for (tbl in c("raw_measurement_registry", "well_metadata", "plate", "dataset"))
      tryCatch(DBI::dbExecute(con, paste0("DELETE FROM ", tbl, " WHERE dataset_id = ?"), list(did)),
               error = function(e) NULL)

    DBI::dbExecute(con, "INSERT INTO dataset
      (dataset_id, raw_file, title, doi, year, authors, notes)
      VALUES (?, ?, ?, ?, ?, ?, ?)", list(
      did,
      basename(f),
      paste0("Waithaka 2022 T. brucei ", sheet_name, " RNAi growth curves"),
      "10.1371/journal.pntd.0010876",
      2022L,
      "Waithaka BG, Ahrens M, Kishore S, Shankarampeta I, Kaufer A, Brecht M, Clayton CE",
      paste0("Cell count growth curves (cumulative cells/mL) for ", sheet_name,
             " RNAi knockdown in T. brucei. Counted by hemocytometer. ",
             "Conditions: Parent (wild-type control) and clones c2/c3/c4 ",
             "with and without tetracycline (tet) induction of the knockdown.")))

    DBI::dbExecute(con, "INSERT INTO plate (plate_id, dataset_id, plate_format, temperature_c)
      VALUES (?, ?, 'flask', 27.0)", list(plate_id, did))

    for (cond in conditions) {
      wid       <- well_id_map[[cond]]
      wnam      <- well_name_map[[cond]]
      is_parent <- grepl("^parent", cond, ignore.case = TRUE)
      tet_on    <- grepl("\\+tet", cond, ignore.case = TRUE)
      clone_m   <- regmatches(cond, regexpr("c[0-9]+", cond, ignore.case = TRUE))
      clone_id  <- if (length(clone_m) > 0L && nchar(clone_m) > 0L) clone_m else NA_character_

      perturb_type <- if (is_parent) "none" else "genetic"
      perturb      <- if (is_parent) "" else
        paste0(sheet_name, " RNAi", if (tet_on) " +tet" else " -tet")
      strain_val   <- if (is_parent) "WT (parent)" else
        paste0(sheet_name, " clone ", ifelse(is.na(clone_id), "?", clone_id))

      DBI::dbExecute(con, "INSERT INTO well_metadata
        (well_id, plate_id, well, species, taxon_id, strain, medium_type,
         perturbation_type, perturbation, is_blank, is_control)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, FALSE, ?)", list(
        wid, plate_id, wnam,
        "Trypanosoma brucei", "5691",
        strain_val, "HMI-9",
        perturb_type, perturb,
        is_parent))
    }

    DBI::dbExecute(con, "INSERT INTO raw_measurement_registry
      (dataset_id, storage_type, path, n_rows, n_wells, signal_type, measurement_unit)
      VALUES (?, 'parquet', ?, ?, ?, 'cell_count', 'cells/ml')",
      list(did, pq_path, nrow(pq_df), length(conditions)))

    tryCatch(qres_sync_conditions(con, did), error = function(e) NULL)
    .wmsg("Done: ", did)
  }
  invisible(NULL)
}

# ─── Puromycin dose–response ──────────────────────────────────────────────────

# Parse a raw + blank-subtracted sheet pair from the tethering Excel.
# df_raw, df_blank: tibbles from read_xlsx(..., col_names=FALSE).
# protein: character label (e.g. "GFP").
# Returns list(wells_df, long_df) or NULL on failure.
.parse_dr_pair <- function(df_raw, df_blank, protein) {
  # Concentrations from header row (cols 3+, numeric)
  hdr       <- suppressWarnings(as.numeric(as.character(unlist(df_raw[1, -(1:2)]))))
  conc_idx  <- which(!is.na(hdr))
  if (length(conc_idx) == 0L) return(NULL)
  concentrations <- hdr[conc_idx]
  raw_cols   <- conc_idx + 2L   # offset for the two metadata cols skipped

  # Same for blank-subtracted (may have fewer cols)
  hdr_b     <- suppressWarnings(as.numeric(as.character(unlist(df_blank[1, -(1:2)]))))
  blank_idx <- which(!is.na(hdr_b))
  blank_cols <- blank_idx + 2L

  # Build a lookup: concentration → col index in blank sheet
  blank_col_map <- setNames(blank_cols, as.character(hdr_b[blank_idx]))

  raw_df   <- as.data.frame(df_raw[-1, ])
  blank_df <- as.data.frame(df_blank[-1, ])

  current_cell_line <- NA_character_
  current_tet       <- NA
  tet_rep           <- 0L
  batch             <- 0L
  last_cl           <- ""

  wells_out <- list()
  long_out  <- list()

  for (r in seq_len(nrow(raw_df))) {
    c1 <- trimws(gsub("[\r\n].*", "", as.character(raw_df[[r, 1]])))
    c2 <- trimws(as.character(raw_df[[r, 2]]))
    if (is.na(c1) || c1 == "NA") c1 <- NA_character_
    if (is.na(c2) || c2 == "NA") c2 <- NA_character_

    if (!is.na(c1)) {
      if (c1 != last_cl) { last_cl <- c1; current_cell_line <- c1; batch <- 0L }
      batch       <- batch + 1L
      tet_rep     <- 0L
      current_tet <- NA
    }
    if (!is.na(c2) && c2 %in% c("-", "+")) {
      current_tet <- (c2 == "+")
      tet_rep     <- 0L
    }
    tet_rep <- tet_rep + 1L
    if (is.na(current_cell_line) || is.na(current_tet)) next

    tet_str  <- if (current_tet) "tet" else "notet"
    cl_clean <- gsub("[^a-zA-Z0-9]", "_", current_cell_line)
    wname    <- sprintf("%s_%s_batch%d_%s_rep%d", protein, cl_clean, batch, tet_str, tet_rep)

    # Fluorescence values
    fl_raw <- suppressWarnings(as.numeric(as.character(
      unlist(raw_df[r, raw_cols[raw_cols <= ncol(raw_df)]]))))

    fl_blank_sub <- rep(NA_real_, length(concentrations))
    for (j in seq_along(concentrations)) {
      ck <- as.character(concentrations[j])
      if (!is.null(blank_col_map[[ck]]) && blank_col_map[[ck]] <= ncol(blank_df)) {
        fl_blank_sub[j] <- suppressWarnings(
          as.numeric(as.character(blank_df[[r, blank_col_map[[ck]]]])))
      }
    }

    wells_out[[length(wells_out) + 1L]] <- data.frame(
      well        = wname,
      protein     = protein,
      cell_line   = current_cell_line,
      batch       = batch,
      tet_induced = current_tet,
      replicate   = tet_rep,
      stringsAsFactors = FALSE)

    long_out[[length(long_out) + 1L]] <- data.frame(
      well        = rep(wname, length(concentrations)),
      conc_ng_ml  = concentrations,
      value       = fl_blank_sub,   # blank-subtracted = primary value
      value_raw   = fl_raw,
      stringsAsFactors = FALSE)
  }

  if (length(wells_out) == 0L) return(NULL)
  list(wells = do.call(rbind, wells_out),
       long  = do.call(rbind, long_out))
}

# Parse one sheet from "Puro doserep_PPT length.xls".
# No tet column — concentrations start at col 2.  The ± boxB is encoded in sheet name.
.parse_ppt_sheet <- function(df, boxb_label) {
  hdr       <- suppressWarnings(as.numeric(as.character(unlist(df[1, -1L]))))
  conc_idx  <- which(!is.na(hdr))
  if (length(conc_idx) == 0L) return(NULL)
  concentrations <- hdr[conc_idx]
  data_cols  <- conc_idx + 1L

  dat <- as.data.frame(df[-1, ])
  current_cl <- NA_character_
  batch      <- 0L
  rep_n      <- 0L
  last_cl    <- ""

  wells_out <- list()
  long_out  <- list()

  for (r in seq_len(nrow(dat))) {
    c1 <- trimws(gsub("[\r\n].*", "", as.character(dat[[r, 1]])))
    if (is.na(c1) || c1 == "NA") c1 <- NA_character_

    if (!is.na(c1)) {
      if (c1 != last_cl) { last_cl <- c1; current_cl <- c1; batch <- 0L }
      batch <- batch + 1L
      rep_n <- 0L
    }
    rep_n <- rep_n + 1L
    if (is.na(current_cl)) next

    cl_clean  <- gsub("[^a-zA-Z0-9]", "_", current_cl)
    box_clean <- if (grepl("^\\+", boxb_label)) "boxBon" else "boxBoff"
    wname <- sprintf("%s_%s_batch%d_rep%d", cl_clean, box_clean, batch, rep_n)

    fl_raw <- suppressWarnings(as.numeric(as.character(
      unlist(dat[r, data_cols[data_cols <= ncol(dat)]]))))

    wells_out[[length(wells_out) + 1L]] <- data.frame(
      well        = wname,
      cell_line   = current_cl,
      boxb        = boxb_label,
      batch       = batch,
      replicate   = rep_n,
      stringsAsFactors = FALSE)

    long_out[[length(long_out) + 1L]] <- data.frame(
      well       = rep(wname, length(concentrations)),
      conc_ng_ml = concentrations,
      value      = fl_raw,
      value_raw  = fl_raw,
      stringsAsFactors = FALSE)
  }

  if (length(wells_out) == 0L) return(NULL)
  list(wells = do.call(rbind, wells_out),
       long  = do.call(rbind, long_out))
}

.waithaka_write_dr_dataset <- function(con, did, long_df, title, notes,
                                        compound, n_wells, n_conc, pq_dir) {
  pq_path <- file.path(pq_dir, paste0(did, ".parquet"))
  arrow::write_parquet(long_df, pq_path)
  .wmsg("Parquet: ", pq_path, " (", nrow(long_df), " rows, ", n_wells, " wells)")

  for (tbl in c("dose_response_registry", "plate", "dataset"))
    tryCatch(DBI::dbExecute(con, paste0("DELETE FROM ", tbl, " WHERE dataset_id = ?"), list(did)),
             error = function(e) NULL)

  DBI::dbExecute(con, "INSERT INTO dataset
    (dataset_id, raw_file, title, doi, year, authors, notes) VALUES (?,?,?,?,?,?,?)", list(
    did, "Puro doseresp", title,
    "10.1371/journal.pntd.0010876", 2022L,
    "Waithaka BG, Ahrens M, Kishore S, Shankarampeta I, Kaufer A, Brecht M, Clayton CE",
    notes))

  DBI::dbExecute(con, "INSERT INTO plate (plate_id, dataset_id, plate_format, temperature_c)
    VALUES (?, ?, '96-well', 27.0)", list(paste0(did, "_p1"), did))

  DBI::dbExecute(con, "INSERT INTO dose_response_registry
    (dataset_id, storage_type, path, compound, conc_unit, signal_type, measurement_unit, n_wells, n_conc)
    VALUES (?, 'parquet', ?, ?, 'ng/mL', 'fluorescence', 'RFU', ?, ?)",
    list(did, pq_path, compound, n_wells, n_conc))
  waithaka_register_dr_wells(con, did)
}

# One well_metadata row per dose-response series (cell line × BoxB × batch × replicate),
# so the dataset appears in dataset_conditions and in search. Concentrations vary within
# a series (Parquet column conc_ng_ml), so concentration stays NULL here.
waithaka_register_dr_wells <- function(con, did) {
  reg <- DBI::dbGetQuery(con, "SELECT path, compound FROM dose_response_registry WHERE dataset_id = ?",
                         list(did))
  if (nrow(reg) == 0L) stop("No dose_response_registry row for ", did)
  series <- unique(as.data.frame(arrow::read_parquet(reg$path[1]))$well)
  pid    <- DBI::dbGetQuery(con, "SELECT plate_id FROM plate WHERE dataset_id = ? LIMIT 1", list(did))$plate_id
  DBI::dbExecute(con, "DELETE FROM well_metadata WHERE plate_id = ?", list(pid))
  wm <- data.frame(
    well_id = paste0(pid, "__", series), plate_id = pid, well = series,
    species = "Trypanosoma brucei", taxon_id = "5691",
    strain = sub("_batch.*$", "", series), medium_state = "liquid", medium_type = "HMI-9",
    perturbation_type = "chemical", perturbation = reg$compound[1],
    concentration = NA_real_, concentration_unit = "ng/mL",
    is_blank = FALSE, is_control = FALSE,
    replicate = suppressWarnings(as.integer(sub(".*_rep", "", series))),
    stringsAsFactors = FALSE)
  if ("dataset_id" %in% DBI::dbListFields(con, "well_metadata")) wm$dataset_id <- did
  DBI::dbAppendTable(con, "well_metadata", wm)
  tryCatch(qres_sync_conditions(con, did), error = function(e) NULL)
  invisible(nrow(wm))
}

waithaka_import_puro_dr <- function(con, source_dir, overwrite = FALSE) {
  # ── Tethering file ─────────────────────────────────────────────────────────
  f_teth <- file.path(source_dir, "Puro doseresp_Tethering.xlsx")
  did_t  <- "waithaka_2022_tbr_puro_tethering"

  if (file.exists(f_teth)) {
    skip <- !overwrite && tryCatch(
      nrow(DBI::dbGetQuery(con, "SELECT 1 FROM dataset WHERE dataset_id = ?", list(did_t))) > 0L,
      error = function(e) FALSE)

    if (!skip) {
      sheets   <- readxl::excel_sheets(f_teth)
      raw_sn   <- grep("^3190", sheets, value = TRUE)
      blank_sn <- grep("sample.*blank|blank.*sample", sheets, ignore.case = TRUE, value = TRUE)

      # Build protein → (raw_sheet, blank_sheet) map
      extract_protein <- function(s) toupper(trimws(gsub("^3190[_-]|_sample.*|sample.*|-blank.*", "", s, ignore.case = TRUE)))
      raw_map   <- setNames(raw_sn,   extract_protein(raw_sn))
      blank_map <- setNames(blank_sn, extract_protein(blank_sn))

      long_all  <- list()
      wells_all <- list()
      pq_dir    <- "data/parquet/dose_response"
      dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)

      for (prot in intersect(names(raw_map), names(blank_map))) {
        .wmsg("Parsing tethering protein: ", prot)
        df_raw   <- readxl::read_xlsx(f_teth, sheet = raw_map[[prot]],   col_names = FALSE)
        df_blank <- readxl::read_xlsx(f_teth, sheet = blank_map[[prot]], col_names = FALSE)
        res <- .parse_dr_pair(df_raw, df_blank, prot)
        if (!is.null(res)) {
          long_all[[prot]]  <- res$long
          wells_all[[prot]] <- res$wells
        }
      }

      if (length(long_all) > 0L) {
        long_df <- do.call(rbind, long_all)
        n_wells <- length(unique(long_df$well))
        n_conc  <- length(unique(long_df$conc_ng_ml))
        long_df <- long_df[, c("well", "conc_ng_ml", "value", "value_raw")]

        .waithaka_write_dr_dataset(
          con, did_t, long_df,
          title    = "Waithaka 2022 T. brucei puromycin dose-response (mRNA tethering candidates)",
          notes    = paste0(
            "Puromycin resistance reporter assay. Fluorescence (blank-subtracted) vs. ",
            "puromycin concentration (ng/mL). Tests whether RNAi knockdown of candidate ",
            "mRNA processing factors (GFP=ctrl, CPSF73, U2AF65, DRBD3, DRBD4, HNRNPFH, ",
            "TRS1, TRSIP, RBSR) affects read-through of the puromycin resistance gene. ",
            "±tetracycline induction of the RBSR1 tethering system."),
          compound = "puromycin",
          n_wells  = n_wells,
          n_conc   = n_conc,
          pq_dir   = pq_dir)
        .wmsg("Done: ", did_t, " (", n_wells, " wells, ", n_conc, " concentrations)")
      }
    } else {
      .wmsg("Skip (exists): ", did_t)
    }
  } else {
    .wmsg("Not found: ", f_teth)
  }

  # ── PPT length file ────────────────────────────────────────────────────────
  f_ppt <- file.path(source_dir, "Puro doserep_PPT length.xls")
  did_p <- "waithaka_2022_tbr_puro_ppt"

  if (file.exists(f_ppt)) {
    skip <- !overwrite && tryCatch(
      nrow(DBI::dbGetQuery(con, "SELECT 1 FROM dataset WHERE dataset_id = ?", list(did_p))) > 0L,
      error = function(e) FALSE)

    if (!skip) {
      sheets   <- readxl::excel_sheets(f_ppt)
      # Pair sheets: "-boxB fluorescence" ↔ "-boxB sample minus blank"
      # "+boxB flourescence values"       ↔ "+boxB sample minus blank"
      fl_sn    <- grep("fluoresc", sheets, ignore.case = TRUE, value = TRUE)
      blank_sn <- grep("blank", sheets, ignore.case = TRUE, value = TRUE)

      pq_dir <- "data/parquet/dose_response"
      dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)

      long_all  <- list()

      for (bl_sheet in blank_sn) {
        boxb_raw    <- ifelse(grepl("^\\+", bl_sheet), "+boxB", "-boxB")
        match_fl <- fl_sn[grepl(ifelse(grepl("^\\+", bl_sheet), "^\\+", "^-"), fl_sn)]
        if (length(match_fl) == 0L) next
        fl_sheet <- match_fl[1L]

        .wmsg("Parsing PPT: fl=", fl_sheet, " blank=", bl_sheet)
        df_fl    <- readxl::read_xls(f_ppt, sheet = fl_sheet, col_names = FALSE)
        df_blank <- readxl::read_xls(f_ppt, sheet = bl_sheet, col_names = FALSE)

        # PPT blank-subtracted sheets may have same structure as fluorescence
        # Try parse_dr_pair first (has tet col), else fall back to ppt parser
        if (!is.na(as.character(df_fl[[1, 2]])) &&
            !suppressWarnings(is.na(as.numeric(as.character(df_fl[[1, 2]]))))) {
          # col 2 is numeric → concentration starts at col 2 (no tet col)
          res <- .parse_ppt_sheet(df_fl, boxb_raw)
        } else {
          res <- .parse_dr_pair(df_fl, df_blank, boxb_raw)
        }
        if (!is.null(res)) long_all[[boxb_raw]] <- res$long
      }

      if (length(long_all) > 0L) {
        long_df <- do.call(rbind, long_all)
        n_wells <- length(unique(long_df$well))
        n_conc  <- length(unique(long_df$conc_ng_ml))
        long_df <- long_df[, c("well", "conc_ng_ml", "value", "value_raw")]

        .waithaka_write_dr_dataset(
          con, did_p, long_df,
          title    = "Waithaka 2022 T. brucei puromycin dose-response (PPT length reporters)",
          notes    = paste0(
            "Puromycin resistance reporter assay. Tests the effect of polypyrimidine tract ",
            "(PPT) length on read-through efficiency. ±boxB tethering system. ",
            "Fluorescence (blank-subtracted where available) vs. puromycin (ng/mL)."),
          compound = "puromycin",
          n_wells  = n_wells,
          n_conc   = n_conc,
          pq_dir   = pq_dir)
        .wmsg("Done: ", did_p, " (", n_wells, " wells, ", n_conc, " concentrations)")
      }
    } else {
      .wmsg("Skip (exists): ", did_p)
    }
  } else {
    .wmsg("Not found: ", f_ppt)
  }

  invisible(NULL)
}

# ─── Wrapper ──────────────────────────────────────────────────────────────────

waithaka_import_all <- function(con, source_dir, overwrite = FALSE) {
  .wmsg("=== Waithaka 2022 import START ===")
  qres_init_schema(con)    # ensure dose_response_registry and all tables exist
  waithaka_import_rbsr(con, source_dir, overwrite)
  waithaka_import_puro_dr(con, source_dir, overwrite)
  .wmsg("=== Waithaka 2022 import DONE ===")
  invisible(NULL)
}
