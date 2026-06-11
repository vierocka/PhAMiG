`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

# Inline info icon with native browser tooltip.  Usage: tip("text")
tip <- function(text, icon_char = 'ℹ') {
  tags$abbr(
    title  = text,
    style  = 'cursor:help; margin-left:4px; color:#888; font-size:11px; border-bottom:1px dotted #aaa;',
    icon_char
  )
}

# Label helper: field name + red star + optional info icon
req_label <- function(label, tooltip = NULL) {
  lbl <- tags$span(tags$span(class = 'required-star', '★ '), label)
  if (!is.null(tooltip)) tagAppendChild(lbl, tip(tooltip)) else lbl
}

# Label helper: optional field + grey text + optional info icon
opt_label <- function(label, tooltip = NULL) {
  lbl <- tags$span(class = 'optional-label', label)
  if (!is.null(tooltip)) tagAppendChild(lbl, tip(tooltip)) else lbl
}

# Parse replicate sequence.
# Accepts three notations:
#   "1, 2, 3, 4"       — comma-separated integers
#   "1:4"              — integer range (equivalent to the above)
#   "rep(1:4, 12)"     — any R rep() expression; evaluated in baseenv()
# Returns integer vector (empty on parse failure).
.parse_replicate_seq <- function(x) {
  if (!nzchar(trimws(x %||% ''))) return(integer(0))
  x <- trimws(x)

  if (grepl('rep\\s*\\(', x, perl = TRUE)) {
    result <- tryCatch(
      as.integer(eval(parse(text = x), envir = baseenv())),
      error   = function(e) integer(0),
      warning = function(w) integer(0)
    )
    return(if (length(result) > 0 && length(result) <= 10000L) result else integer(0))
  }

  parts <- trimws(strsplit(x, ',')[[1]])
  parts <- parts[nzchar(parts)]
  out <- integer(0)
  for (p in parts) {
    if (grepl('^\\d+:\\d+$', trimws(p))) {
      ends <- as.integer(strsplit(trimws(p), ':')[[1]])
      out <- c(out, ends[1]:ends[2])
    } else {
      v <- suppressWarnings(as.integer(trimws(p)))
      if (!is.na(v)) out <- c(out, v)
    }
  }
  out
}

# Expand one well expression token. Handles:
#   A1          → A1
#   A1:H1       → A1,B1,C1,D1,E1,F1,G1,H1  (same column, row range)
#   A1:A12      → A1,A2,…,A12               (same row, col range)
#   A1:12       → A1,A2,…,A12               (same row, just col end)
#   A:12        → A1,A2,…,A12               (row only + col end)
#   E12:H12     → E12,F12,G12,H12           (same col number, row range)
#   c(A1,B1)    → A1,B1                     (R-vector literal)
#   Plate prefix Px: or Plate x: stripped silently (per-plate not yet supported)
.expand_well_token <- function(p) {
  p <- trimws(toupper(p))
  if (!nzchar(p)) return(character(0))
  # Strip plate prefix:  P1:  /  PLATE1:  /  P 2:
  p <- sub('^(?:PLATE)?\\s*P?\\d+\\s*:', '', p, perl = TRUE, ignore.case = TRUE)
  p <- trimws(p)
  if (!nzchar(p)) return(character(0))
  # c(…) notation
  if (grepl('^C\\s*\\(', p)) {
    inner <- sub('^C\\s*\\((.*)\\)\\s*$', '\\1', p, perl = TRUE)
    return(toupper(trimws(strsplit(inner, '[,;]')[[1]])))
  }
  if (!grepl(':', p, fixed = TRUE)) {
    if (grepl('^[A-Z]+[0-9]+$', p)) return(p)
    return(character(0))
  }
  parts2 <- strsplit(p, ':', fixed = TRUE)[[1]]
  if (length(parts2) != 2) return(character(0))
  w1 <- trimws(parts2[1]); w2 <- trimws(parts2[2])
  row1 <- regmatches(w1, regexpr('^[A-Z]+', w1))
  col1 <- suppressWarnings(as.integer(regmatches(w1, regexpr('[0-9]+$', w1))))
  row2 <- regmatches(w2, regexpr('^[A-Z]+', w2))
  col2 <- suppressWarnings(as.integer(regmatches(w2, regexpr('[0-9]+$', w2))))
  hr1 <- length(row1) > 0 && nzchar(row1); hc1 <- length(col1) > 0 && !is.na(col1)
  hr2 <- length(row2) > 0 && nzchar(row2); hc2 <- length(col2) > 0 && !is.na(col2)
  # Same col number, different rows: A1:H1 or E12:H12
  if (hr1 && hc1 && hr2 && hc2 && col1 == col2 && row1 != row2) {
    i1 <- match(row1, LETTERS); i2 <- match(row2, LETTERS)
    if (!is.na(i1) && !is.na(i2)) return(paste0(LETTERS[seq(min(i1,i2),max(i1,i2))], col1))
  }
  # Same row, different cols: A1:A12 or A1:B1 with same row
  if (hr1 && hc1 && hr2 && hc2 && row1 == row2) {
    return(paste0(row1, seq(min(col1,col2), max(col1,col2))))
  }
  # A1:12 — same row, col end only
  if (hr1 && hc1 && !hr2 && hc2) {
    return(paste0(row1, seq(min(col1,col2), max(col1,col2))))
  }
  # A:12 — row only + col end
  if (hr1 && !hc1 && !hr2 && hc2) {
    return(paste0(row1, seq(1L, col2)))
  }
  character(0)
}

# Parse a free-form well list into a character vector of normalised well names.
# Accepts comma/semicolon-separated tokens, colon-range notation, R c() literals,
# and optional plate prefixes (P1:, Plate2:) which are stripped (applies to all plates).
# If `available` is supplied, only wells present in it are returned.
.parse_wells <- function(x, available = NULL) {
  if (!nzchar(trimws(x %||% ''))) return(character(0))
  x <- gsub('[;\n\r]', ',', x)
  x <- gsub('\\s*,\\s*', ',', trimws(x))
  # Protect commas inside c(…) literals so they aren't used as token separators.
  # Replace each interior comma one at a time until none remain inside any c(…).
  HOLD <- '\x01'
  while (grepl('[Cc]\\([^)]*,[^)]*\\)', x))
    x <- sub('([Cc]\\([^)]*),([^)]*\\))', paste0('\\1', HOLD, '\\2'), x)
  toks <- trimws(strsplit(x, ',')[[1]])
  toks <- toks[nzchar(toks)]
  toks <- gsub(HOLD, ',', toks, fixed = TRUE)
  out  <- unique(toupper(unlist(lapply(toks, .expand_well_token))))
  out  <- out[grepl('^[A-Z]+[0-9]+$', out)]
  if (!is.null(available)) out <- intersect(out, toupper(available))
  out
}

# Parse a time column to decimal hours.
#
# Colon-separated formats (auto-detected by part count):
#   dd:hh:mm:ss  → days · 24 + hours + minutes/60 + seconds/3600
#   hh:mm:ss     → hours + minutes/60 + seconds/3600
#   mm:ss        → minutes/60 + seconds/3600
#
# Plain numeric values are returned as-is (assumed to already be in hours).
# Unrecognised values become NA.
.parse_time_to_hours <- function(x) {
  x <- as.character(x)
  num <- suppressWarnings(as.numeric(x))
  if (mean(is.na(num), na.rm = TRUE) <= 0.1) return(num)   # already numeric (hours)

  vapply(x, function(v) {
    v <- trimws(v)
    if (is.na(v) || !nzchar(v)) return(NA_real_)
    n <- suppressWarnings(as.numeric(v))
    if (!is.na(n)) return(n)
    parts <- suppressWarnings(as.numeric(strsplit(v, ':', fixed = TRUE)[[1]]))
    if (any(is.na(parts))) return(NA_real_)
    np <- length(parts)
    if (np == 2L) return(parts[1]/60 + parts[2]/3600)                            # mm:ss
    if (np == 3L) return(parts[1] + parts[2]/60 + parts[3]/3600)                 # hh:mm:ss
    if (np == 4L) return(parts[1]*24 + parts[2] + parts[3]/60 + parts[4]/3600)   # dd:hh:mm:ss
    NA_real_
  }, numeric(1), USE.NAMES = FALSE)
}

# Classify well content labels to blank / control / sample.
# Returns 'blank', 'control', or 'sample'.
# Covers common plate-reader auto-generated labels.
.classify_well_label <- function(labels) {
  lbl <- tolower(trimws(as.character(labels)))
  blank_pat   <- 'only.medium|^medium$|^blank$|blanks?$|only.med$|^lb$|^lb.only$|no.bacteria|no.?inocul'
  control_pat <- 'no.stressor|no.drug|only.inocul|^control$|^ctrl$|pos.?control|neg.?control|wt.?only'
  dplyr::case_when(
    grepl(blank_pat,   lbl, perl = TRUE) ~ 'blank',
    grepl(control_pat, lbl, perl = TRUE) ~ 'control',
    TRUE ~ 'sample'
  )
}

qres_clean_undefined <- function(x) {
  x <- as.character(x)
  x[x %in% c('', 'undefined', 'NA', 'NaN', 'NULL')] <- NA_character_
  x
}

qres_new_id <- function(..., sep = '__') paste(..., sep = sep)

qres_safe_numeric <- function(x) suppressWarnings(as.numeric(x))
