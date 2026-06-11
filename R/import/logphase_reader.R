# LogPhase600 (BioTek) multi-plate Excel reader.
#
# Recognises files where each plate occupies three sheets:
#   "Plate N - Results"   — per-well descriptors + pre-computed metrics
#   "Plate N - Raw Data"  — wide time-series (time col already decimal hours)
#   "Plate N - Procedure" — one-cell procedure summary with all settings
#
# Public API:
#   qres_detect_logphase_format(path)        → TRUE / FALSE
#   qres_parse_procedure_text(txt)           → named list
#   qres_read_logphase_procedure(path)       → named list (first plate's settings)
#   qres_read_logphase_raw(path, dataset_id) → long data.frame (same schema as qres_raw_to_long)
#   qres_read_logphase_well_meta(path, dataset_id) → data.frame (well × metadata cols)

# Returns TRUE when at least one "Plate N - Raw Data" sheet exists.
qres_detect_logphase_format <- function(path) {
  if (!requireNamespace('readxl', quietly = TRUE)) return(FALSE)
  if (!grepl('\\.xlsx?$', path, ignore.case = TRUE)) return(FALSE)
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
  any(grepl('^Plate\\s+\\d+\\s+-\\s+Raw Data$', sheets, ignore.case = TRUE))
}

# ── Procedure text parser ─────────────────────────────────────────────────────
#
# Input:  the raw procedure summary string (double-space-delimited key: value pairs)
# Output: named list with fields:
#   plate_type, plate_number, runtime_raw, interval_raw, n_reads,
#   wavelength_nm, temperature_c, temperature_gradient,
#   shaking, shaking_rpm, incubator
qres_parse_procedure_text <- function(txt) {
  if (is.null(txt) || is.na(txt) || !nzchar(trimws(txt))) return(list())

  # Split on two-or-more consecutive spaces → individual "Key: Value" segments
  segs <- trimws(strsplit(txt, '\\s{2,}')[[1]])
  segs <- segs[nzchar(segs)]

  # Build a key → value map for colon-delimited segments
  kv <- list()
  for (s in segs) {
    m <- regexpr(':', s, fixed = TRUE)
    if (m > 0) {
      k <- trimws(substring(s, 1, m - 1))
      v <- trimws(substring(s, m + 1))
      kv[[tolower(k)]] <- v
    }
  }

  out <- list()

  # Plate Type
  if (!is.null(kv[['plate type']]))
    out$plate_type <- kv[['plate type']]

  # Plate Number
  if (!is.null(kv[['plate number']]))
    out$plate_number <- suppressWarnings(as.integer(kv[['plate number']]))

  # Discontinuous Kinetics block: "Runtime HH:MM:SS …, Interval HH:MM:SS …, N Reads …"
  kinetics_raw <- kv[['discontinuous kinetics']] %||% ''
  m <- regmatches(kinetics_raw, regexpr('Runtime\\s+(\\d+:\\d+:\\d+)', kinetics_raw, perl = TRUE))
  if (length(m)) out$runtime_raw <- sub('Runtime\\s+', '', m)

  m <- regmatches(kinetics_raw, regexpr('Interval\\s+(\\d+:\\d+:\\d+)', kinetics_raw, perl = TRUE))
  if (length(m)) out$interval_raw <- sub('Interval\\s+', '', m)

  m <- regmatches(kinetics_raw, regexpr('(\\d+)\\s+Reads', kinetics_raw, perl = TRUE))
  if (length(m)) out$n_reads <- suppressWarnings(as.integer(sub('\\s+Reads.*', '', m)))

  # Wavelength: "Read: 600 nm"
  read_v <- kv[['read']] %||% ''
  m <- regmatches(read_v, regexpr('\\d+', read_v))
  if (length(m)) out$wavelength_nm <- as.integer(m)

  # Temperature setpoint
  temp_v <- kv[['temperature setpoint']] %||% ''
  m <- regmatches(temp_v, regexpr('[0-9]+(?:\\.[0-9]+)?', temp_v, perl = TRUE))
  if (length(m)) out$temperature_c <- as.numeric(m)

  # Temperature gradient
  tg_v <- kv[['temperature gradient']] %||% NA_character_
  if (!is.na(tg_v)) out$temperature_gradient <- tolower(trimws(tg_v)) == 'on'

  # Shaking
  sh_v <- kv[['shaking']] %||% NA_character_
  if (!is.na(sh_v)) out$shaking <- tolower(trimws(sh_v)) == 'on'

  # Shaking frequency
  sf_v <- kv[['shaking frequency']] %||% ''
  m <- regmatches(sf_v, regexpr('\\d+', sf_v))
  if (length(m)) out$shaking_rpm <- as.integer(m)

  # Incubator
  inc_v <- kv[['incubator']] %||% NA_character_
  if (!is.na(inc_v)) out$incubator <- tolower(trimws(inc_v)) == 'on'

  out
}

# Read the Procedure text from the first "Plate N - Procedure" sheet and parse it.
qres_read_logphase_procedure <- function(path) {
  sheets  <- readxl::excel_sheets(path)
  proc_sh <- sheets[grepl('^Plate\\s+\\d+\\s+-\\s+Procedure$', sheets, ignore.case = TRUE)]
  if (length(proc_sh) == 0) return(list())
  df  <- suppressMessages(
    as.data.frame(readxl::read_excel(path, sheet = proc_sh[[1]], col_names = FALSE)))
  txt <- df[[1, 2]]
  qres_parse_procedure_text(txt)
}

# ── Raw data reader ───────────────────────────────────────────────────────────
# Reads all "Plate N - Raw Data" sheets and returns a combined long data.frame.
# The first row of each sheet is the header (Time, A1, A2, …).
# Time column is already stored as character decimal hours (no POSIXct issue).
qres_read_logphase_raw <- function(path, dataset_id = '__upload__', signal_type = 'OD600') {
  requireNamespace('data.table')
  sheets   <- readxl::excel_sheets(path)
  raw_shs  <- sheets[grepl('^Plate\\s+\\d+\\s+-\\s+Raw Data$', sheets, ignore.case = TRUE)]
  nums     <- as.integer(regmatches(raw_shs, regexpr('\\d+', raw_shs)))
  raw_shs  <- raw_shs[order(nums)]; nums <- sort(nums)

  long_list <- lapply(seq_along(raw_shs), function(i) {
    plate_id <- paste0(dataset_id, '__plate_', nums[[i]])
    df <- suppressMessages(
      as.data.frame(readxl::read_excel(path, sheet = raw_shs[[i]], col_names = FALSE,
                                        .name_repair = 'minimal')))
    names(df) <- as.character(unlist(df[1, ]))
    df  <- df[-1, , drop = FALSE]
    df  <- .fix_excel_df(df)
    tmp <- tempfile(fileext = '.csv')
    utils::write.csv(df, tmp, row.names = FALSE)
    dt  <- .strip_empty(data.table::fread(tmp, fill = TRUE))
    time_col <- names(dt)[grepl('time', names(dt), ignore.case = TRUE)][[1]]
    if (is.na(time_col)) return(NULL)
    long <- data.table::melt(dt, id.vars = time_col,
                              variable.name = 'well', value.name = 'value',
                              variable.factor = FALSE)
    data.table::setnames(long, time_col, 'time_h')
    long[, time_h     := .parse_time_to_hours(as.character(time_h))]
    long[, dataset_id  := dataset_id]
    long[, plate_id    := plate_id]
    long[, well_id     := paste(dataset_id, plate_id, well, sep = '__')]
    long[, signal_type := signal_type]
    data.table::setcolorder(long, c('dataset_id', 'plate_id', 'well_id',
                                     'well', 'time_h', 'signal_type', 'value'))
    long[]
  })
  long_list <- Filter(Negate(is.null), long_list)
  if (length(long_list) == 0L) return(NULL)
  as.data.frame(data.table::rbindlist(long_list))
}

# ── Well metadata reader ──────────────────────────────────────────────────────
# Reads all "Plate N - Results" sheets.
# Returns a data.frame with columns:
#   well, plate_id, replicate, [strain/perturbation cols…],
#   is_blank, is_control, [pre-computed metric cols…]
#
# Column name normalisation:
#   - Any column named "replicate*"        → replicate (integer)
#   - Any column containing "strain"        → strain
#   - Any column containing "concentration" → concentration (numeric)
#   - "only medium" in strain column        → is_blank = TRUE
#   - Lagtime, Max Rate, Stationary Phase   → kept as-is (pre-computed metrics)
qres_read_logphase_well_meta <- function(path, dataset_id = '__upload__') {
  sheets  <- readxl::excel_sheets(path)
  res_shs <- sheets[grepl('^Plate\\s+\\d+\\s+-\\s+Results$', sheets, ignore.case = TRUE)]
  nums    <- as.integer(regmatches(res_shs, regexpr('\\d+', res_shs)))
  res_shs <- res_shs[order(nums)]; nums <- sort(nums)

  meta_list <- lapply(seq_along(res_shs), function(i) {
    plate_id <- paste0(dataset_id, '__plate_', nums[[i]])
    df <- suppressMessages(
      as.data.frame(readxl::read_excel(path, sheet = res_shs[[i]], col_names = FALSE,
                                        .name_repair = 'minimal')))
    hdrs <- trimws(as.character(unlist(df[1, ])))
    df   <- df[-1, , drop = FALSE]
    names(df) <- hdrs

    # Normalise well column
    well_col <- names(df)[grepl('^well$', names(df), ignore.case = TRUE)][[1]]
    df$well  <- trimws(as.character(df[[well_col]]))

    # Normalise strain column → strain
    strain_col <- names(df)[grepl('strain', names(df), ignore.case = TRUE)]
    strain_col <- strain_col[strain_col != well_col]
    if (length(strain_col)) df$strain <- trimws(as.character(df[[strain_col[[1]]]]))

    # Normalise concentration column → concentration (numeric)
    conc_col <- names(df)[grepl('concentration', names(df), ignore.case = TRUE)]
    if (length(conc_col))
      df$concentration <- suppressWarnings(as.numeric(as.character(df[[conc_col[[1]]]])))

    # Normalise replicate column → replicate (integer)
    rep_col <- names(df)[grepl('^replicate', names(df), ignore.case = TRUE)]
    if (length(rep_col))
      df$replicate <- suppressWarnings(as.integer(as.numeric(as.character(df[[rep_col[[1]]]]))))

    # is_blank / is_control: classify via shared label pattern function
    if ('strain' %in% names(df)) {
      cls <- .classify_well_label(df$strain)
      df$is_blank   <- cls == 'blank'
      df$is_control <- cls == 'control'
    } else {
      df$is_blank   <- FALSE
      df$is_control <- FALSE
    }

    df$is_control <- FALSE
    df$plate_id   <- plate_id

    # Keep pre-computed metric columns (Lagtime, Max Rate, Stationary Phase)
    metric_cols <- names(df)[grepl('lagtime|max.?rate|stationary', names(df), ignore.case = TRUE)]

    keep <- unique(c('well', 'plate_id', 'strain', 'concentration', 'replicate',
                     'is_blank', 'is_control', metric_cols))
    keep <- keep[keep %in% names(df)]
    df[, keep, drop = FALSE]
  })
  dplyr::bind_rows(meta_list)
}
