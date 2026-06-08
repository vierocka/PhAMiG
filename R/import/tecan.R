# Tecan Spark / Infinite plate reader CSV parser.
# Tecan exports have a metadata block, label rows, and sometimes multiple reads
# per file. This parser extracts the first OD table.

qres_import_tecan <- function(con, csv_path, dataset_id,
                               signal_type = 'OD600',
                               metadata_csv = 'data/metaData.csv') {
  lines      <- readLines(csv_path)
  # Tecan data blocks start after a "<>" or "Cycle Nr." header line
  data_start <- which(grepl('Cycle Nr\\.|^<>', lines))[1]
  if (is.na(data_start)) {
    # Fall back to generic parser
    return(qres_register_raw_measurements(con, dataset_id, csv_path,
                                           signal_type  = signal_type,
                                           metadata_csv = metadata_csv))
  }

  tmp <- tempfile(fileext = '.csv')
  writeLines(lines[data_start:length(lines)], tmp)
  on.exit(unlink(tmp))

  qres_register_raw_measurements(con, dataset_id, tmp,
                                  signal_type  = signal_type,
                                  metadata_csv = metadata_csv)
}
