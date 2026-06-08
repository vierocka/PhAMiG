qres_reset_tables <- function(con) {
  for (tbl in c('dataset', 'plate', 'well_metadata', 'raw_measurement_registry',
                 'processing_run', 'derived_metric', 'model_fit')) {
    if (DBI::dbExistsTable(con, tbl)) DBI::dbExecute(con, paste('DELETE FROM', tbl))
  }
  invisible(TRUE)
}
