qres_connect <- function(db_path = 'data/qres.duckdb', read_only = FALSE) {
  requireNamespace('DBI')
  requireNamespace('duckdb')
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
}

qres_disconnect <- function(con, shutdown = TRUE) {
  DBI::dbDisconnect(con, shutdown = shutdown)
}
