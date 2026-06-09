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

# Parse a well list with comma separation and colon-range notation.
# "A1, H12, A12:E12"  →  c("A1","H12","A12","B12","C12","D12","E12")
# A12:E12 = same column (12), rows A–E.   A1:A6 = same row (A), columns 1–6.
.parse_wells <- function(x) {
  if (!nzchar(trimws(x %||% ''))) return(character(0))
  parts <- parts <- trimws(strsplit(x, ',')[[1]])
  parts <- parts[nzchar(parts)]
  out <- character(0)
  for (p in parts) {
    if (grepl(':', p, fixed = TRUE)) {
      ends <- toupper(trimws(strsplit(p, ':')[[1]]))
      if (length(ends) == 2) {
        row1 <- gsub('[0-9]+', '', ends[1]); col1 <- suppressWarnings(as.integer(gsub('[A-Z]+', '', ends[1])))
        row2 <- gsub('[0-9]+', '', ends[2]); col2 <- suppressWarnings(as.integer(gsub('[A-Z]+', '', ends[2])))
        if (!is.na(col1) && !is.na(col2) && nzchar(row1) && nzchar(row2)) {
          if (col1 == col2) {
            i1 <- match(row1, LETTERS); i2 <- match(row2, LETTERS)
            if (!is.na(i1) && !is.na(i2)) out <- c(out, paste0(LETTERS[i1:i2], col1))
          } else if (row1 == row2) {
            out <- c(out, paste0(row1, col1:col2))
          }
        }
      }
    } else {
      out <- c(out, toupper(p))
    }
  }
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

qres_clean_undefined <- function(x) {
  x <- as.character(x)
  x[x %in% c('', 'undefined', 'NA', 'NaN', 'NULL')] <- NA_character_
  x
}

qres_new_id <- function(..., sep = '__') paste(..., sep = sep)

qres_safe_numeric <- function(x) suppressWarnings(as.numeric(x))
