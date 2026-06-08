# BioTek plate reader CSV parser.
# BioTek exports include a multi-line header block before the data table.
# This parser strips the header and delegates to qres_raw_to_long().

qres_import_biotek <- function(con, csv_path, dataset_id,
                                signal_type = 'OD600',
                                metadata_csv = 'data/metaData.csv') {
  lines <- readLines(csv_path, n = 60)
  data_start <- which(grepl('^Time|^0:', lines))[1]
  if (is.na(data_start)) stop('Could not detect data start in BioTek file: ', csv_path)

  tmp <- tempfile(fileext = '.csv')
  writeLines(lines[data_start:length(readLines(csv_path))], tmp)
  on.exit(unlink(tmp))

  qres_register_raw_measurements(con, dataset_id, tmp,
                                  signal_type  = signal_type,
                                  metadata_csv = metadata_csv)
}
