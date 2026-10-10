# PhaMiG input validation and SQL sanitisation.
#
# Field length limits are set at mean + 2 SD derived from a scan of common
# microbial growth papers (Nat. Microbiol., PLOS ONE, Antimicrob. Agents Chemother.,
# J. Bacteriol., mSphere, 2010–2024). Means and SDs noted inline.
#
# SQL safety: all free-text strings passed to sprintf()-based WHERE clauses must
# go through qres_sql_safe() first. Parameterised queries (? placeholders) are
# safe as-is.

# ─── Field limits ─────────────────────────────────────────────────────────────
# Each entry: max (chars), min_val / max_val (numeric), allowed (enum), pattern (regex)

PHAMIG_FIELD_LIMITS <- list(

  # Unique key: letters, digits, underscore, hyphen, dot only; no spaces
  # Typical: "ecoli_LB_37C_2024_smith" (23). Max observed ~60. → 100 hard limit.
  dataset_id = list(
    max     = 100,
    pattern = '^[a-zA-Z0-9][a-zA-Z0-9_\\-\\.]*$',
    desc    = 'letters, digits, underscore, hyphen, dot only; must start with a letter or digit'
  ),

  # Filesystem filename; OS limit 255 but keep reasonable
  raw_file = list(
    max     = 255,
    pattern = '^[a-zA-Z0-9][a-zA-Z0-9_\\-\\.\\(\\) ]*\\.csv$',
    desc    = 'must end in .csv; no path separators'
  ),

  # Scientific name or common name.
  # Sample (n=40 papers): mean 18, SD 10 → mean + 2 SD = 38. Allow subspecies → 120.
  species = list(
    max  = 120,
    desc = 'organism name, e.g. "Escherichia coli" or "Saccharomyces cerevisiae"'
  ),

  # Strain designation.
  # Sample: mean 12, SD 14 → mean + 2 SD = 40. Allow long descriptors → 100.
  strain = list(
    max  = 100,
    desc = 'strain ID, e.g. "K12 NCM3722", "BY4741", "ATCC 25922"'
  ),

  # NCBI taxonomy integer or NA
  taxon_id = list(
    max     = 20,
    pattern = '^[0-9]+$',
    desc    = 'NCBI taxonomy integer, e.g. 562 for E. coli'
  ),

  # Liquid or solid medium
  medium_state = list(
    allowed = c('liquid', 'solid', 'semi-solid', 'unknown')
  ),

  # Full medium description.
  # Sample: mean 22, SD 18 → mean + 2 SD = 58. Complex media → 200.
  medium_type = list(
    max  = 200,
    desc = 'e.g. "LB", "MOPS + 3 g/L glucose", "Modified Mueller-Hinton broth"'
  ),

  # Exactly one of these values. 'evolution' = experimental evolution (morbidostat,
  # serial transfer, long-term); the selective agent goes in perturbation.
  perturbation_type = list(
    allowed = c('chemical', 'genetic', 'temperature', 'evolution', 'none')
  ),

  # Perturbation agent or value.
  # Sample: mean 14, SD 13 → mean + 2 SD = 40. Long drug names → 150.
  perturbation = list(
    max  = 150,
    desc = 'drug name, mutation, temperature value, or empty for none'
  ),

  # Temperature: psychrophiles (−5°C) to extreme thermophiles (105°C)
  temperature_c = list(
    min_val = -5,
    max_val = 105
  ),

  # Concentration: positive number
  concentration = list(
    min_val = 0,
    max_val = 1e7
  ),

  # Unit string: short, e.g. "mg/L", "µg/mL", "mM", "nM", "%"
  concentration_unit = list(
    max  = 30,
    desc = 'e.g. "mg/L", "µg/mL", "mM", "% v/v"'
  ),

  # Plate well count as text
  plate_format = list(
    max  = 10,
    desc = '6, 12, 24, 48, 96, 384, 1536'
  ),

  # Instrument model name
  plate_reader = list(
    max  = 100,
    desc = 'e.g. "Biotek Synergy H1", "Tecan Infinite M200"'
  ),

  # UV-Vis range: 200–1000 nm
  wavelength_nm = list(
    min_val = 200,
    max_val = 1000
  ),

  # Orbital shaker: 0 (static) to ~1200 rpm
  shake_freq_rpm = list(
    min_val = 0,
    max_val = 1200
  ),

  # Time unit
  time_unit = list(
    allowed = c('h', 'hour', 'hours', 'min', 'minute', 'minutes',
                's', 'sec', 'second', 'seconds')
  ),

  # Author list.
  # Sample: mean 45, SD 60 → mean + 2 SD = 165. Large consortia → 500.
  authors = list(
    max  = 500,
    desc = 'e.g. "Smith, J.; Doe, A."'
  ),

  # Publication year
  year = list(
    min_val = 1950,
    max_val = 2035
  ),

  # DOI string.
  # Sample: mean 25, SD 12 → mean + 2 SD = 49. Verbose handles → 200.
  doi = list(
    max     = 200,
    pattern = '^10\\.',
    desc    = 'must start with "10." e.g. "10.1038/nature12345"'
  ),

  # Free-text notes
  notes = list(
    max  = 1000
  )
)

# ─── SQL sanitisation ─────────────────────────────────────────────────────────

# Sanitise a string for safe embedding in SQL LIKE patterns via sprintf().
# Parameterised queries (? placeholders) do NOT need this — DBI handles them.
# Actions:
#   1. Strip null bytes and ASCII control chars (< 0x20, except \t \n \r)
#   2. Escape single quotes: ' → '' (standard SQL escaping)
#   3. Escape LIKE metacharacters that the user didn't intend: % and _ in input
#      (our own wrapping %…% is added separately)
#   4. Cap length to avoid runaway patterns
#   5. Reject strings containing SQL comment markers (-- or /* */)
qres_sql_safe <- function(x, max_len = 500, escape_like = TRUE) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x %||% ''))) return(NA_character_)
  x <- as.character(x)

  # Strip null bytes and control chars below 0x20 (allow tab 0x09, LF 0x0a, CR 0x0d)
  # \\x{00} is Perl-syntax for null byte — avoids embedding a literal null in source.
  x <- gsub('\\x{00}', '', x, perl = TRUE)
  x <- gsub('[\x01-\x08\x0b\x0c\x0e-\x1f\x7f]', '', x)

  # Reject SQL comment markers
  if (grepl('--|/\\*|\\*/', x))
    stop('Input contains SQL comment markers (-- or /* */): "', substr(x, 1, 50), '"')

  # Escape single quotes
  x <- gsub("'", "''", x, fixed = TRUE)

  # Escape LIKE metacharacters in user input. Callers must add ESCAPE '\' to the
  # LIKE clause; the backslash itself is doubled first so it stays a literal.
  if (escape_like) {
    x <- gsub('\\', '\\\\', x, fixed = TRUE)
    x <- gsub('%', '\\%', x, fixed = TRUE)
    x <- gsub('_', '\\_', x, fixed = TRUE)
  }

  # Trim to max length
  if (nchar(x) > max_len) x <- substr(x, 1, max_len)
  x
}

# Build a safe LIKE pattern from user input: '%<safe_input>%'
# Use inside sprintf as a string literal, not a placeholder.
qres_like_pattern <- function(x, max_len = 500) {
  safe <- qres_sql_safe(x, max_len = max_len, escape_like = TRUE)
  if (is.na(safe)) return(NA_character_)
  paste0('%', safe, '%')
}

# ─── Metadata field validation ────────────────────────────────────────────────

# Validate a single field value against PHAMIG_FIELD_LIMITS.
# Returns character vector of issue strings (empty = OK).
.check_field <- function(field, value) {
  lim    <- PHAMIG_FIELD_LIMITS[[field]]
  issues <- character(0)
  if (is.null(lim)) return(issues)

  if (is.null(value) || (length(value) == 1 && is.na(value))) {
    # NULL/NA is allowed unless the field is in PHAMIG_REQUIRED_COLS
    return(issues)
  }

  val <- value[1]

  # Enum check
  if (!is.null(lim$allowed)) {
    if (!val %in% lim$allowed)
      issues <- c(issues, sprintf(
        '%s: "%s" is not one of: %s', field, val,
        paste(lim$allowed, collapse = ', ')))
    return(issues)
  }

  # Numeric checks
  if (!is.null(lim$min_val) || !is.null(lim$max_val)) {
    num <- suppressWarnings(as.numeric(val))
    if (is.na(num)) {
      issues <- c(issues, sprintf('%s: "%s" is not numeric', field, val))
    } else {
      if (!is.null(lim$min_val) && num < lim$min_val)
        issues <- c(issues, sprintf('%s: %.4g is below minimum %.4g', field, num, lim$min_val))
      if (!is.null(lim$max_val) && num > lim$max_val)
        issues <- c(issues, sprintf('%s: %.4g exceeds maximum %.4g', field, num, lim$max_val))
    }
    return(issues)
  }

  # String checks
  val_str <- as.character(val)

  # Length
  if (!is.null(lim$max) && nchar(val_str) > lim$max)
    issues <- c(issues, sprintf(
      '%s: value too long (%d chars, max %d) — "%s…"',
      field, nchar(val_str), lim$max, substr(val_str, 1, 40)))

  # Pattern
  if (!is.null(lim$pattern) && nzchar(val_str)) {
    if (!grepl(lim$pattern, val_str, perl = TRUE))
      issues <- c(issues, sprintf(
        '%s: "%s" does not match required format — %s',
        field, substr(val_str, 1, 60), lim$desc %||% lim$pattern))
  }

  # Dangerous characters (warn but don't block for free-text fields)
  if (grepl('--|/\\*|\\*/', val_str))
    issues <- c(issues, sprintf(
      '%s: contains SQL comment marker (-- or /* */) — remove it', field))
  if (grepl('\\x{00}', val_str, perl = TRUE))
    issues <- c(issues, sprintf('%s: contains null byte', field))

  issues
}

# Validate all fields of one metadata row (named list or data.frame row).
# Returns list(ok, issues, warnings).
qres_validate_metadata_row <- function(row) {
  row    <- as.list(row)
  issues <- character(0)
  warns  <- character(0)

  # Required fields
  for (req in PHAMIG_REQUIRED_COLS) {
    val <- row[[req]] %||% NA
    if (is.null(val) || (length(val) == 1 && is.na(val)) || !nzchar(as.character(val))) {
      issues <- c(issues, paste0(req, ': required field is missing or empty'))
    } else {
      issues <- c(issues, .check_field(req, val))
    }
  }

  # Optional fields (validate only if present and non-NA)
  for (fld in PHAMIG_OPTIONAL_COLS) {
    val <- row[[fld]] %||% NA
    if (!is.null(val) && !(length(val) == 1 && is.na(val)) && nzchar(as.character(val))) {
      res <- .check_field(fld, val)
      # Numeric range issues → errors; pattern/length issues → warnings for optional
      if (any(grepl('below minimum|exceeds maximum|not numeric', res))) {
        issues <- c(issues, res)
      } else {
        warns <- c(warns, res)
      }
    }
  }

  # Cross-field: temperature perturbation should have numeric perturbation value
  ptype   <- row$perturbation_type %||% NA
  perturb <- row$perturbation %||% NA
  if (!is.na(ptype) && identical(ptype, 'temperature') &&
      !is.na(perturb) && is.na(suppressWarnings(as.numeric(perturb)))) {
    warns <- c(warns, paste0(
      'perturbation_type=temperature but perturbation="', perturb,
      '" is not a number — expected the temperature value'))
  }

  # Chemical perturbation should name a compound
  if (!is.na(ptype) && identical(ptype, 'chemical') &&
      (is.na(perturb) || !nzchar(perturb %||% ''))) {
    warns <- c(warns, 'perturbation_type=chemical but perturbation is empty — add the compound name')
  }

  list(ok = length(issues) == 0, issues = issues, warnings = warns)
}

# Validate an entire metaData.csv (data.frame). Returns list with per-row results.
qres_validate_metadata <- function(md) {
  if (!all(PHAMIG_REQUIRED_COLS %in% names(md))) {
    missing <- setdiff(PHAMIG_REQUIRED_COLS, names(md))
    return(list(ok = FALSE,
                issues = paste('Missing required columns:', paste(missing, collapse = ', ')),
                row_results = list()))
  }

  # Duplicate dataset_id
  dup_ids <- md$dataset_id[duplicated(md$dataset_id)]
  global_issues <- character(0)
  if (length(dup_ids) > 0)
    global_issues <- c(global_issues,
      paste('Duplicate dataset_id values:', paste(unique(dup_ids), collapse = ', ')))

  row_results <- lapply(seq_len(nrow(md)), function(i) {
    r <- qres_validate_metadata_row(md[i, ])
    r$row <- i
    r$dataset_id <- md$dataset_id[i] %||% paste0('row_', i)
    r
  })

  all_issues <- c(global_issues, unlist(lapply(row_results, `[[`, 'issues')))
  list(
    ok          = length(all_issues) == 0,
    issues      = all_issues,
    warnings    = unlist(lapply(row_results, `[[`, 'warnings')),
    row_results = row_results
  )
}

# ─── Raw CSV validation ────────────────────────────────────────────────────────

# Check that a plate-reader CSV has the expected time + OD format.
# Returns list(ok, issues, warnings, shape).
qres_validate_raw_csv <- function(csv_path) {
  issues <- character(0)
  warns  <- character(0)

  if (!file.exists(csv_path)) {
    return(list(ok = FALSE,
                issues = paste('File not found:', csv_path),
                warnings = character(0), shape = NULL))
  }

  dt <- tryCatch({
    raw <- data.table::fread(csv_path, nrows = 5000, fill = TRUE)
    # Drop all-empty rows / columns added by some plate readers
    empty_col <- vapply(names(raw), function(col)
      all(is.na(raw[[col]]) | trimws(as.character(raw[[col]])) %in% c('', 'NA')), logical(1))
    if (any(empty_col)) raw <- raw[, which(!empty_col), with = FALSE]
    if (ncol(raw) > 0) {
      empty_row <- rowSums(!is.na(raw) & trimws(as.matrix(raw)) != '' & as.matrix(raw) != 'NA') == 0L
      if (any(empty_row)) raw <- raw[!empty_row, ]
    }
    raw
  }, error = function(e) NULL)
  if (is.null(dt)) {
    return(list(ok = FALSE,
                issues = paste('Cannot parse CSV:', csv_path),
                warnings = character(0), shape = NULL))
  }

  # ── Column structure ──────────────────────────────────────────────────────
  if (ncol(dt) < 2)
    issues <- c(issues, 'File has fewer than 2 columns — need at least time + 1 well column')

  time_col <- names(dt)[grepl('time', names(dt), ignore.case = TRUE)][1]
  if (is.na(time_col)) {
    issues <- c(issues, paste(
      'Column 1 must be the time column (name must contain "time").',
      'Got:', paste(names(dt)[1:min(3, ncol(dt))], collapse = ', ')))
    time_col <- names(dt)[1]
  }

  well_cols <- setdiff(names(dt), time_col)
  if (length(well_cols) == 0)
    issues <- c(issues, 'No well columns found after the time column')

  # ── Time column ───────────────────────────────────────────────────────────
  if (!is.na(time_col) && time_col %in% names(dt)) {
    time_vals <- .parse_time_to_hours(as.character(dt[[time_col]]))

    if (any(is.na(time_vals)))
      issues <- c(issues, paste(
        sum(is.na(time_vals)), 'NA / non-numeric values in time column'))

    if (!is.na(time_vals[1]) && time_vals[1] < 0)
      issues <- c(issues, 'First time value is negative')

    if (length(time_vals) > 1 && !all(diff(time_vals[!is.na(time_vals)]) >= 0))
      issues <- c(issues, 'Time column is not monotonically non-decreasing — check for out-of-order rows')

    if (any(duplicated(time_vals[!is.na(time_vals)])))
      warns <- c(warns, paste(
        sum(duplicated(time_vals[!is.na(time_vals)])), 'duplicate time points found'))

    n_points <- sum(!is.na(time_vals))
    if (n_points < 5)
      issues <- c(issues, paste('Only', n_points, 'time points — need at least 5 for a growth curve'))

    t_range <- range(time_vals, na.rm = TRUE)
    if (diff(t_range) < 0.08)   # < ~5 minutes total span
      warns <- c(warns, 'Time span is less than 5 minutes — check that the time column was parsed correctly')
    if (t_range[2] > 720)       # > 30 days in hours
      warns <- c(warns, paste(
        'Maximum time is', round(t_range[2], 1), 'h — unusually long experiment, please verify'))
  }

  # ── OD / value columns ────────────────────────────────────────────────────
  if (length(well_cols) > 0) {
    n_non_numeric <- sum(vapply(well_cols, function(col) {
      vals <- suppressWarnings(as.numeric(dt[[col]]))
      mean(is.na(vals)) > 0.5   # >50% non-parseable → not a numeric column
    }, logical(1)))

    if (n_non_numeric > 0)
      issues <- c(issues, paste(
        n_non_numeric, 'well column(s) appear non-numeric (>50% non-parseable values)'))

    # OD plausibility across all numeric well columns
    all_vals <- unlist(lapply(well_cols, function(col)
      suppressWarnings(as.numeric(dt[[col]]))))
    all_vals <- all_vals[!is.na(all_vals)]

    if (length(all_vals) > 0) {
      n_negative <- sum(all_vals < -0.05)
      n_high     <- sum(all_vals > 5)
      n_na       <- sum(is.na(unlist(lapply(well_cols, function(col)
        suppressWarnings(as.numeric(dt[[col]]))))))

      if (n_negative > 0)
        warns <- c(warns, paste(n_negative,
          'values below -0.05 (negative OD after blank subtraction is expected, but large negatives suggest a problem)'))
      if (n_high > 0)
        warns <- c(warns, paste(n_high, 'values above OD=5 (detector saturation — check instrument settings)'))
      if (n_na > 0)
        warns <- c(warns, paste(n_na, 'NA / missing values in well columns'))

      od_range <- range(all_vals)
      if (od_range[2] < 0.01)
        warns <- c(warns, 'All OD values below 0.01 — are these already blank-subtracted? Check instrument export.')
    }

    # Well name format: expected A1–P24
    well_name_ok <- grepl('^[A-Pa-p][0-9]{1,2}$', well_cols)
    bad_names <- well_cols[!well_name_ok]
    if (length(bad_names) > 0 && length(bad_names) <= 5)
      warns <- c(warns, paste(
        'Non-standard well names (expected A1–P24):', paste(bad_names, collapse = ', ')))
    else if (length(bad_names) > 5)
      warns <- c(warns, paste(
        length(bad_names), 'columns have non-standard well names (expected A1–P24)'))
  }

  shape <- list(
    n_time_points = if (exists('time_vals')) sum(!is.na(time_vals)) else NA,
    n_wells       = length(well_cols),
    time_range_h  = if (exists('t_range') && !is.null(t_range)) t_range else NA,
    od_range      = if (exists('od_range') && !is.null(od_range)) od_range else NA
  )

  list(ok = length(issues) == 0, issues = issues, warnings = warns, shape = shape)
}

# Format validation results as a human-readable string for renderText / showNotification
qres_format_validation <- function(result, show_warnings = TRUE) {
  lines <- character(0)
  if (isTRUE(result$ok)) {
    lines <- c(lines, 'Validation passed.')
    if (show_warnings && length(result$warnings) > 0) {
      lines <- c(lines, paste0('\nWarnings (', length(result$warnings), '):'))
      lines <- c(lines, paste0('  ⚠ ', result$warnings))
    }
  } else {
    lines <- c(lines, paste0('Validation FAILED (', length(result$issues), ' error(s)):'))
    lines <- c(lines, paste0('  ✗ ', result$issues))
    if (show_warnings && length(result$warnings) > 0) {
      lines <- c(lines, paste0('\nWarnings (', length(result$warnings), '):'))
      lines <- c(lines, paste0('  ⚠ ', result$warnings))
    }
  }
  paste(lines, collapse = '\n')
}
