# Baseline correction applied to raw_long before plotting or fitting.
# method: 'none' | 'min_well' | 'first_3' | 'blank_wells'
# blank_ids: well_ids with is_blank=TRUE (needed for 'blank_wells' only)
#
# References:
#   Sprouffske & Wagner (2012) doi:10.1111/j.1558-5646.2012.01583.x  — min per well
#   Hall et al. (2014) doi:10.1093/molbev/msu069                     — first N timepoints
#   Mira et al. (2022) / gcplyr blank parameter                      — blank wells
.apply_baseline <- function(raw_long, method = 'none', blank_ids = character(0)) {
  if (is.null(method) || method == 'none') return(raw_long)
  requireNamespace('dplyr')
  if (method == 'min_well') {
    raw_long <- raw_long |>
      dplyr::group_by(well_id) |>
      dplyr::mutate(value = value - min(value, na.rm = TRUE)) |>
      dplyr::ungroup()
  } else if (method == 'first_3') {
    raw_long <- raw_long |>
      dplyr::group_by(well_id) |>
      dplyr::arrange(time_h) |>
      dplyr::mutate(value = value - mean(utils::head(value, 3), na.rm = TRUE)) |>
      dplyr::ungroup()
  } else if (method == 'blank_wells' && length(blank_ids) > 0) {
    # Per-dataset subtraction when multiple datasets are present prevents
    # cross-experiment blank contamination (different experiments may have
    # different baseline OD levels and incompatible time axes).
    if ('dataset_id' %in% names(raw_long) && dplyr::n_distinct(raw_long$dataset_id) > 1L) {
      blank_mean <- raw_long |>
        dplyr::filter(well_id %in% blank_ids) |>
        dplyr::group_by(dataset_id, time_h) |>
        dplyr::summarise(blank_val = mean(value, na.rm = TRUE), .groups = 'drop')
      raw_long <- raw_long |>
        dplyr::left_join(blank_mean, by = c('dataset_id', 'time_h')) |>
        dplyr::mutate(value = value - dplyr::coalesce(blank_val, 0)) |>
        dplyr::select(-blank_val)
    } else {
      blank_mean <- raw_long |>
        dplyr::filter(well_id %in% blank_ids) |>
        dplyr::group_by(time_h) |>
        dplyr::summarise(blank_val = mean(value, na.rm = TRUE), .groups = 'drop')
      raw_long <- raw_long |>
        dplyr::left_join(blank_mean, by = 'time_h') |>
        dplyr::mutate(value = value - dplyr::coalesce(blank_val, 0)) |>
        dplyr::select(-blank_val)
    }
  }
  raw_long
}

# Apply baseline correction and remove blank wells.
# Shared by Analyze Data and NL Query so blank handling is identical everywhere.
#
# blank_names: well short names (e.g. 'A1') from manual text input; resolved to
#              well_ids via raw_long so cross-dataset contamination is impossible
# blank_ids:   well_ids already resolved (from DB or auto-detect)
qres_prepare_curves <- function(raw_long, method = 'none',
                                blank_names = character(0),
                                blank_ids   = character(0)) {
  if (is.null(raw_long) || nrow(raw_long) == 0) return(raw_long)
  name_ids      <- if (length(blank_names) > 0 && 'well' %in% names(raw_long))
    unique(raw_long$well_id[raw_long$well %in% blank_names]) else character(0)
  all_blank_ids <- union(blank_ids, name_ids)
  raw_long      <- .apply_baseline(raw_long, method = method, blank_ids = all_blank_ids)
  if (length(all_blank_ids) > 0)
    raw_long <- raw_long[!raw_long$well_id %in% all_blank_ids, , drop = FALSE]
  raw_long
}

qres_plot_growth <- function(raw_long, log_y = FALSE, colour_by = 'well',
                              control_wells = character(0), title = NULL,
                              rep_groups = list()) {
  requireNamespace('ggplot2')

  if (isTRUE(log_y)) {
    raw_long <- raw_long[is.finite(raw_long$value) & raw_long$value > 0, ]
    if (nrow(raw_long) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  # Rep-group colouring: overrides colour_by when replicate groups are assigned.
  # rep_groups: named list well → integer group ID.
  # Legend labels: "R1 (A1, B1, …)", "R2 (C1, D1, …)", plus ungrouped wells
  # in grey. Controls remain black.
  if (length(rep_groups) > 0 && 'well' %in% names(raw_long)) {
    .REP_PALETTE <- c('#e6a817','#3498db','#2ecc71','#e74c3c',
                      '#9b59b6','#1abc9c','#e67e22','#16a085',
                      '#8e44ad','#2980b9','#27ae60','#d35400')
    rep_df <- data.frame(well      = names(rep_groups),
                         rep_group = as.integer(unlist(rep_groups)),
                         stringsAsFactors = FALSE)
    raw_long <- dplyr::left_join(raw_long, rep_df, by = 'well')
    # Build label map: group → "R1 (A1, B1, …)"
    grp_labels <- tapply(rep_df$well, rep_df$rep_group, function(ws) {
      ws <- sort(ws); lbl <- paste(head(ws, 4), collapse = ', ')
      if (length(ws) > 4) lbl <- paste0(lbl, ', …')
      lbl
    })
    raw_long$.colour_grp <- dplyr::case_when(
      'well_id' %in% names(raw_long) & raw_long$well_id %in% control_wells ~ 'Control',
      !is.na(raw_long$rep_group) ~ paste0('R', raw_long$rep_group,
                                           ' (', grp_labels[as.character(raw_long$rep_group)], ')'),
      TRUE ~ 'Other'
    )
    grp_ids   <- sort(unique(raw_long$rep_group[!is.na(raw_long$rep_group)]))
    grp_names <- paste0('R', grp_ids, ' (', grp_labels[as.character(grp_ids)], ')')
    pal_vals  <- setNames(.REP_PALETTE[seq_along(grp_ids)], grp_names)
    extras    <- setNames(c('black', '#aaaaaa'), c('Control', 'Other'))
    extras    <- extras[names(extras) %in% unique(raw_long$.colour_grp)]
    pal       <- c(pal_vals, extras)
    show_legend <- length(grp_ids) > 0
    p <- ggplot2::ggplot(raw_long,
           ggplot2::aes(x = time_h, y = value,
                        group = well_id, colour = .colour_grp,
                        text  = well_id)) +
      ggplot2::geom_line(alpha = 0.6, linewidth = 0.45) +
      ggplot2::scale_color_manual(values = pal, name = 'Replicate group') +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::labs(x = 'Time [h]', y = unique(raw_long$signal_type)[1], title = title) +
      ggplot2::theme(legend.position = if (show_legend) 'right' else 'none')
    if (isTRUE(log_y)) p <- p + ggplot2::scale_y_log10()
    return(p)
  }

  multi_dataset <- colour_by != 'well' && colour_by %in% names(raw_long) &&
                   length(unique(raw_long[[colour_by]])) > 1

  if (length(control_wells) > 0 && 'well_id' %in% names(raw_long)) {
    grp_col <- if (multi_dataset && colour_by %in% names(raw_long))
      raw_long[[colour_by]] else raw_long$well
    raw_long$.colour_grp <- ifelse(raw_long$well_id %in% control_wells, 'Control', grp_col)
    non_ctrl <- setdiff(unique(raw_long$.colour_grp), 'Control')
    pal <- setNames(c('black', scales::hue_pal()(length(non_ctrl))),
                    c('Control', non_ctrl))
    p <- ggplot2::ggplot(raw_long,
           ggplot2::aes(x = time_h, y = value,
                        group = well_id, colour = .colour_grp, text = well_id)) +
      ggplot2::geom_line(alpha = 0.55, linewidth = 0.4) +
      ggplot2::scale_color_manual(values = pal, name = NULL) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::labs(x = 'Time [h]', y = unique(raw_long$signal_type)[1], title = title) +
      ggplot2::theme(legend.position = 'right')
    if (isTRUE(log_y)) p <- p + ggplot2::scale_y_log10()
    return(p)
  }

  p <- ggplot2::ggplot(raw_long,
         ggplot2::aes(
           x      = time_h,
           y      = value,
           group  = if (multi_dataset) interaction(.data[[colour_by]], well) else well,
           colour = if (multi_dataset) .data[[colour_by]] else well,
           text   = well_id
         )) +
    ggplot2::geom_line(alpha = 0.55, linewidth = 0.4) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(x = 'Time [h]',
                  y = unique(raw_long$signal_type)[1],
                  colour = if (multi_dataset) colour_by else NULL,
                  title  = title) +
    ggplot2::theme(legend.position = if (multi_dataset) 'right' else 'none')
  if (isTRUE(log_y)) p <- p + ggplot2::scale_y_log10()
  p
}

# Overlay curves from multiple datasets, coloured by temperature
qres_plot_multi_temp <- function(raw_long_list, log_y = FALSE) {
  requireNamespace('ggplot2')
  requireNamespace('dplyr')
  combined <- dplyr::bind_rows(raw_long_list, .id = 'source_idx')
  ggplot2::ggplot(combined,
    ggplot2::aes(x = time_h, y = value,
                 group = interaction(dataset_id, well),
                 colour = dataset_id)) +
    ggplot2::geom_line(alpha = 0.35, linewidth = 0.35) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(x = 'Time [h]',
                  y = if ('signal_type' %in% names(combined)) unique(combined$signal_type)[1] else 'OD',
                  colour = 'Dataset / Temperature') +
    if (isTRUE(log_y)) ggplot2::scale_y_log10() else ggplot2::scale_y_continuous()
}
