# Bootstrap the Qres DB without launching Shiny.
# Run from repository root:  Rscript bootstrap_demo.R

source('R/utils/helpers.R')
source('R/db/connect.R')
source('R/db/schema.R')
source('R/db/migrations.R')
source('R/import/metadata.R')
source('R/import/generic_csv.R')

con <- qres_connect('data/qres.duckdb')
qres_init_schema(con)
qres_reset_tables(con)

# ── 1. Import all metadata ──────────────────────────────────────────────────
cat('Importing metadata…\n')
qres_import_metadata(con, 'data/metaData.csv')
cat(sprintf('  dataset table: %d rows\n',
            DBI::dbGetQuery(con, 'SELECT COUNT(*) AS n FROM dataset')$n))

# ── 2. Register raw measurements for every available CSV ──────────────────
md <- data.table::fread('data/metaData.csv')
cat(sprintf('Registering %d raw CSV files…\n', nrow(md)))

for (i in seq_len(nrow(md))) {
  csv_path <- file.path('data/raw_csv', md$File[i])
  if (!file.exists(csv_path)) {
    cat(sprintf('  SKIP (not found): %s\n', md$File[i]))
    next
  }
  tryCatch({
    qres_register_raw_measurements(con,
      dataset_id = md$Stamp[i],
      raw_csv    = csv_path)
    cat(sprintf('  OK  %s  (%s)\n', md$Stamp[i], md$File[i]))
  }, error = function(e) {
    cat(sprintf('  ERR %s: %s\n', md$Stamp[i], conditionMessage(e)))
  })
}

# ── 3. Sanity checks ────────────────────────────────────────────────────────
cat('\n── Dataset summary ─────────────────────────────────────────────────\n')
print(DBI::dbGetQuery(con, "
  SELECT d.dataset_id, w.species, p.temperature_c,
         CASE WHEN rmr.path IS NOT NULL THEN 'YES' ELSE 'no' END AS has_raw
  FROM dataset d
  LEFT JOIN plate p USING(dataset_id)
  LEFT JOIN (SELECT plate_id, species FROM well_metadata GROUP BY plate_id, species) w USING(plate_id)
  LEFT JOIN raw_measurement_registry rmr USING(dataset_id)
  ORDER BY p.temperature_c
"))

cat('\n── Raw measurement registry ─────────────────────────────────────────\n')
print(DBI::dbGetQuery(con, 'SELECT dataset_id, storage_type, n_wells, n_rows FROM raw_measurement_registry ORDER BY dataset_id'))

qres_disconnect(con)
cat('\nDone.\n')
