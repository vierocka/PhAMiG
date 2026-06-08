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

qres_clean_undefined <- function(x) {
  x <- as.character(x)
  x[x %in% c('', 'undefined', 'NA', 'NaN', 'NULL')] <- NA_character_
  x
}

qres_new_id <- function(..., sep = '__') paste(..., sep = sep)

qres_safe_numeric <- function(x) suppressWarnings(as.numeric(x))
