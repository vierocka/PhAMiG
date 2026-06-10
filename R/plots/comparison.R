# Cross-tool metric comparison plots.
# Input: long-format data frame from qres_compare_plugin_outputs()
# with columns: dataset_id, well_id, metric_name, metric_value, tool

QRES_KEY_METRICS <- c('mumax_percapita', 'lag_time_h', 'doubling_time_h', 'auc_trapezoid')

.TOOL_LABELS <- c(
  model_free  = 'model_free',
  gcplyr      = 'gcplyr',
  growthrates = 'growthrates',
  dgrothr     = 'DGrowthR',
  kinbiont    = 'KinBiont'
)

.relabel_tools <- function(df) {
  if ('tool' %in% names(df))
    df$tool <- ifelse(df$tool %in% names(.TOOL_LABELS),
                      .TOOL_LABELS[df$tool], df$tool)
  df
}

qres_plot_metric_comparison <- function(comparison_df, metric = 'mumax_percapita',
                                         facet_by = NULL) {
  requireNamespace('ggplot2')
  requireNamespace('dplyr')

  df <- dplyr::filter(comparison_df, metric_name == metric, is.finite(metric_value))
  df <- .relabel_tools(df)
  if (nrow(df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate('text', x=.5, y=.5, label=paste('No data for metric:', metric)) +
      ggplot2::theme_void())
  }

  unit_label <- unique(df$metric_unit)[1] %||% ''
  y_label    <- if (nzchar(unit_label)) paste0(metric, ' [', unit_label, ']') else metric

  p <- ggplot2::ggplot(df,
         ggplot2::aes(x = tool, y = metric_value, fill = tool, colour = tool)) +
    ggplot2::geom_boxplot(alpha = 0.35, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.55, size = 1.2) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(x = 'Tool', y = y_label,
                  title = paste('Cross-tool comparison:', metric)) +
    ggplot2::theme(legend.position = 'none',
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

  if (!is.null(facet_by) && facet_by %in% names(df)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste('~', facet_by)),
                                   scales = 'free_y')
  }
  p
}

# Wide pivot: rows = well_id, cols = tool, values = metric_value for one metric
qres_pivot_comparison <- function(comparison_df, metric = 'mumax_percapita') {
  requireNamespace('tidyr')
  requireNamespace('dplyr')
  comparison_df |>
    dplyr::filter(metric_name == metric) |>
    dplyr::select(dataset_id, well_id, tool, metric_value) |>
    tidyr::pivot_wider(names_from = tool, values_from = metric_value)
}

# Pairwise scatter: tool A vs tool B for one metric
qres_plot_tool_scatter <- function(comparison_df, tool_x, tool_y,
                                    metric = 'mumax_percapita') {
  requireNamespace('ggplot2')
  wide <- qres_pivot_comparison(.relabel_tools(comparison_df), metric)
  lx <- .TOOL_LABELS[tool_x] %||% tool_x
  ly <- .TOOL_LABELS[tool_y] %||% tool_y
  # column names after relabelling
  cx <- if (lx %in% names(wide)) lx else tool_x
  cy <- if (ly %in% names(wide)) ly else tool_y
  if (!all(c(cx, cy) %in% names(wide))) {
    return(ggplot2::ggplot() +
      ggplot2::annotate('text', x=.5, y=.5, label='Tool not available in results') +
      ggplot2::theme_void())
  }
  ggplot2::ggplot(wide, ggplot2::aes(x = .data[[cx]], y = .data[[cy]])) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 'dashed', colour = 'grey50') +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(x = paste(lx, '[', metric, ']'),
                  y = paste(ly, '[', metric, ']'),
                  title = paste(lx, 'vs', ly, '—', metric))
}
