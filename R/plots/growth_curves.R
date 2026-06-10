qres_plot_growth <- function(raw_long, log_y = FALSE, colour_by = 'well',
                              title = NULL) {
  requireNamespace('ggplot2')

  multi_dataset <- colour_by != 'well' && colour_by %in% names(raw_long) &&
                   length(unique(raw_long[[colour_by]])) > 1

  p <- ggplot2::ggplot(raw_long,
         ggplot2::aes(
           x      = time_h,
           y      = value,
           group  = if (multi_dataset)
                      interaction(.data[[colour_by]], well)
                    else
                      well,
           colour = if (multi_dataset)
                      .data[[colour_by]]
                    else
                      well
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
