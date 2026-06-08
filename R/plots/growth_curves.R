qres_plot_growth <- function(raw_long, log_y = FALSE, colour_by = 'well',
                              title = NULL) {
  requireNamespace('ggplot2')
  p <- ggplot2::ggplot(raw_long,
         ggplot2::aes(x = time_h, y = value, group = well,
                      colour = if (colour_by == 'well') well else get(colour_by))) +
    ggplot2::geom_line(alpha = 0.55, linewidth = 0.4) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(x = 'Time [h]',
                  y = unique(raw_long$signal_type)[1],
                  colour = colour_by,
                  title  = title) +
    ggplot2::theme(legend.position = 'none')
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
