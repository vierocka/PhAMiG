# Validate long-format plate data (after qres_raw_to_long()).
# Also wraps qres_validate_raw_csv() for pre-parse checks on the raw CSV file.

# Check long-format data (time_h, value, well columns).
# Returns list(ok, issues, warnings, n_wells, n_rows, time_range, od_range).
qres_check_plate_data <- function(raw_long) {
  issues <- character(0)
  warns  <- character(0)

  required <- c('time_h', 'value', 'well')
  missing  <- setdiff(required, names(raw_long))
  if (length(missing) > 0) {
    issues <- c(issues, paste('Missing required columns:', paste(missing, collapse = ', ')))
    return(list(ok = FALSE, issues = issues, warnings = warns,
                n_wells = NA, n_rows = nrow(raw_long), time_range = NA, od_range = NA))
  }

  n_wells   <- length(unique(raw_long$well))
  n_rows    <- nrow(raw_long)
  t_vals    <- raw_long$time_h
  od_vals   <- raw_long$value
  t_range   <- range(t_vals, na.rm = TRUE)
  od_range  <- range(od_vals, na.rm = TRUE)

  # ── Time checks ────────────────────────────────────────────────────────────
  n_time_na <- sum(is.na(t_vals))
  if (n_time_na > 0)
    issues <- c(issues, paste(n_time_na, 'NA values in time_h column'))

  if (any(t_vals < 0, na.rm = TRUE))
    issues <- c(issues, 'Negative time values found — time must start at 0 or later')

  # Monotone per well
  non_monotone_wells <- character(0)
  for (w in unique(raw_long$well)) {
    sub <- raw_long[raw_long$well == w, ]
    sub <- sub[order(sub$time_h), ]
    if (any(diff(sub$time_h) < 0, na.rm = TRUE))
      non_monotone_wells <- c(non_monotone_wells, w)
  }
  if (length(non_monotone_wells) > 0)
    issues <- c(issues, paste(
      'Non-monotone time in wells:', paste(non_monotone_wells[seq_len(min(5, length(non_monotone_wells)))], collapse = ', '),
      if (length(non_monotone_wells) > 5) paste0('... and ', length(non_monotone_wells) - 5, ' more') else ''))

  # Duplicate time points per well
  dup_time_wells <- character(0)
  for (w in unique(raw_long$well)) {
    sub <- raw_long[raw_long$well == w, ]
    if (any(duplicated(sub$time_h[!is.na(sub$time_h)])))
      dup_time_wells <- c(dup_time_wells, w)
  }
  if (length(dup_time_wells) > 0)
    warns <- c(warns, paste(
      'Duplicate time points in wells:', paste(dup_time_wells[seq_len(min(5, length(dup_time_wells)))], collapse = ', '),
      if (length(dup_time_wells) > 5) paste0('... and ', length(dup_time_wells) - 5, ' more') else ''))

  # Minimum time points per well
  min_n_points <- min(tapply(raw_long$time_h, raw_long$well,
                             function(x) sum(!is.na(x))))
  if (min_n_points < 5)
    issues <- c(issues, paste('Some wells have fewer than 5 time points (min:', min_n_points, ')'))

  # Time span
  if (is.finite(diff(t_range))) {
    if (diff(t_range) < 0.5)
      warns <- c(warns, paste('Time span is', round(diff(t_range), 2), 'h — check time is in hours, not minutes'))
    if (t_range[2] > 300)
      warns <- c(warns, paste('Max time is', round(t_range[2], 1), 'h — unusually long; check units'))
  }

  # ── OD / value checks ──────────────────────────────────────────────────────
  n_na  <- sum(is.na(od_vals))
  n_neg <- sum(od_vals < -0.05, na.rm = TRUE)
  n_hi  <- sum(od_vals > 5, na.rm = TRUE)

  if (n_na > 0)
    warns <- c(warns, paste(n_na, 'NA values in OD column'))
  if (n_neg > 0)
    warns <- c(warns, paste(n_neg, 'values below -0.05 (negative OD after blank subtraction)'))
  if (n_hi > 0)
    warns <- c(warns, paste(n_hi, 'values above OD=5 (possible detector saturation)'))
  if (!is.na(od_range[2]) && od_range[2] < 0.01)
    warns <- c(warns, 'All OD values below 0.01 — may not be blank-corrected correctly')

  # ── Structure checks ───────────────────────────────────────────────────────
  if (n_wells < 2)
    warns <- c(warns, 'Only 1 well — typical plates have 2+ wells')

  points_per_well <- table(raw_long$well)
  if (var(as.numeric(points_per_well)) > 0)
    warns <- c(warns, 'Wells have unequal numbers of time points (possible missing rows)')

  list(
    ok         = length(issues) == 0,
    issues     = issues,
    warnings   = warns,
    n_wells    = n_wells,
    n_rows     = n_rows,
    time_range = t_range,
    od_range   = od_range
  )
}
