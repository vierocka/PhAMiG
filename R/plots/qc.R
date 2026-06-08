# QC plots: replicate CV, blank-subtracted traces, signal range heatmap

qres_plot_qc_cv <- function(raw_long) {
  requireNamespace('ggplot2')
  requireNamespace('dplyr')

  cv_df <- raw_long |>
    dplyr::group_by(dataset_id, time_h) |>
    dplyr::summarise(
      mean_od = mean(value, na.rm = TRUE),
      sd_od   = sd(value,   na.rm = TRUE),
      cv      = sd_od / mean_od * 100,
      .groups = 'drop'
    )

  ggplot2::ggplot(cv_df, ggplot2::aes(x = time_h, y = cv)) +
    ggplot2::geom_line(colour = '#E07B39') +
    ggplot2::geom_hline(yintercept = 20, linetype = 'dashed', colour = 'red') +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(x = 'Time [h]', y = 'CV across wells [%]',
                  title = 'Well-to-well CV over time (>20% flags poor replicate agreement)',
                  caption = 'Dashed line: 20% CV threshold')
}

qres_plot_endpoint_heatmap <- function(raw_long, plate_rows = LETTERS[1:8],
                                        plate_cols = 1:12) {
  requireNamespace('ggplot2')
  requireNamespace('dplyr')

  endpoint <- raw_long |>
    dplyr::group_by(well) |>
    dplyr::summarise(od_end = dplyr::last(value[order(time_h)]), .groups = 'drop') |>
    dplyr::mutate(
      row_label = sub('[0-9]+', '', well),
      col_label = as.integer(sub('[A-Za-z]+', '', well))
    ) |>
    dplyr::filter(row_label %in% plate_rows, col_label %in% plate_cols)

  ggplot2::ggplot(endpoint, ggplot2::aes(x = factor(col_label), y = row_label, fill = od_end)) +
    ggplot2::geom_tile(colour = 'white', linewidth = 0.5) +
    ggplot2::scale_fill_viridis_c(option = 'plasma', name = 'OD endpoint') +
    ggplot2::scale_y_discrete(limits = rev(plate_rows)) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::labs(x = 'Column', y = 'Row', title = 'Plate endpoint OD heatmap')
}
