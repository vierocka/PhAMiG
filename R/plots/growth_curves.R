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
    blank_mean <- raw_long |>
      dplyr::filter(well_id %in% blank_ids) |>
      dplyr::group_by(time_h) |>
      dplyr::summarise(blank_val = mean(value, na.rm = TRUE), .groups = 'drop')
    raw_long <- raw_long |>
      dplyr::left_join(blank_mean, by = 'time_h') |>
      dplyr::mutate(value = value - dplyr::coalesce(blank_val, 0)) |>
      dplyr::select(-blank_val)
  }
  raw_long
}

qres_plot_growth <- function(raw_long, log_y = FALSE, colour_by = 'well',
                              control_wells = character(0), title = NULL) {
  requireNamespace('ggplot2')

  if (isTRUE(log_y))
    raw_long <- raw_long[is.finite(raw_long$value) & raw_long$value > 0, ]

  multi_dataset <- colour_by != 'well' && colour_by %in% names(raw_long) &&
                   length(unique(raw_long[[colour_by]])) > 1

  if (length(control_wells) > 0 && 'well_id' %in% names(raw_long)) {
    grp_col <- if (multi_dataset && colour_by %in% names(raw_long))
      raw_long[[colour_by]] else raw_long$well
    raw_long$.colour_grp <- ifelse(raw_long$well_id %in% control_wells,
                                   'Control', grp_col)
    non_ctrl <- setdiff(unique(raw_long$.colour_grp), 'Control')
    pal <- setNames(c('black', scales::hue_pal()(length(non_ctrl))),
                    c('Control', non_ctrl))
    p <- ggplot2::ggplot(raw_long,
           ggplot2::aes(x = time_h, y = value,
                        group = well_id, colour = .colour_grp)) +
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
           colour = if (multi_dataset) .data[[colour_by]] else well
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
    ggplot2::labs(x = 'Time [h]', y = 'OD', colour = 'Dataset / Temperature') +
    if (isTRUE(log_y)) ggplot2::scale_y_log10() else ggplot2::scale_y_continuous()
}
