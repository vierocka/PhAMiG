qres_init_schema <- function(con) {

  # ── dataset: one row per uploaded CSV file ────────────────────────────────
  # dataset_id = unique key; raw_file = the CSV filename (links to raw data)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dataset (
      dataset_id   TEXT PRIMARY KEY,
      raw_file     TEXT NOT NULL,
      title        TEXT,
      doi          TEXT,
      year         INTEGER,
      authors      TEXT,
      license      TEXT,
      source_url   TEXT,
      time_unit    TEXT,
      notes        TEXT,
      import_date  TIMESTAMP DEFAULT current_timestamp
    )
  ")

  # ── plate: instrument / run conditions (one row per dataset) ─────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS plate (
      plate_id          TEXT PRIMARY KEY,
      dataset_id        TEXT NOT NULL,
      plate_format      TEXT,
      plate_colour      TEXT,
      lid               BOOLEAN,
      plate_reader      TEXT,
      spectrophotometer TEXT,
      wavelength_nm     DOUBLE,
      temperature_c     DOUBLE,
      shake_freq_rpm    DOUBLE
    )
  ")

  # ── well_metadata: biological conditions per well ─────────────────────────
  # perturbation_type: chemical | genetic | temperature | none
  # perturbation:      drug name | mutation | temperature value | (empty)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS well_metadata (
      well_id            TEXT PRIMARY KEY,
      plate_id           TEXT NOT NULL,
      well               TEXT,
      species            TEXT NOT NULL,
      taxon_id           TEXT,
      strain             TEXT,
      medium_state       TEXT,
      medium_type        TEXT NOT NULL,
      perturbation_type  TEXT NOT NULL,
      perturbation       TEXT,
      concentration      DOUBLE,
      concentration_unit TEXT,
      is_blank           BOOLEAN DEFAULT FALSE,
      is_control         BOOLEAN DEFAULT FALSE,
      replicate          INTEGER
    )
  ")

  # ── raw_measurement_registry: where the Parquet files live ───────────────
  # signal_type: OD600 | OD750 | fluorescence | luminescence | CFU | cell_count
  # measurement_unit: OD | RFU | RLU | CFU/ml | cells/ml
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS raw_measurement_registry (
      dataset_id        TEXT PRIMARY KEY,
      storage_type      TEXT,
      path              TEXT,
      n_rows            BIGINT,
      n_wells           INTEGER,
      signal_type       TEXT,
      measurement_unit  TEXT,
      created_at        TIMESTAMP DEFAULT current_timestamp
    )
  ")

  # ── processing_run: one row per plugin execution ──────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS processing_run (
      processing_id    TEXT PRIMARY KEY,
      dataset_id       TEXT,
      tool_name        TEXT,
      tool_version     TEXT,
      correction_type  TEXT,
      parameters_json  TEXT,
      status           TEXT,
      message          TEXT,
      container_hash   TEXT,
      git_commit       TEXT,
      created_at       TIMESTAMP DEFAULT current_timestamp
    )
  ")

  # ── derived_metric: one row per metric per well per processing run ────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS derived_metric (
      metric_id     TEXT PRIMARY KEY,
      processing_id TEXT,
      dataset_id    TEXT,
      well_id       TEXT,
      metric_name   TEXT,
      metric_value  DOUBLE,
      metric_unit   TEXT,
      model_name    TEXT
    )
  ")

  # ── model_fit: parameter estimates from parametric models ─────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS model_fit (
      fit_id             TEXT PRIMARY KEY,
      processing_id      TEXT,
      dataset_id         TEXT,
      well_id            TEXT,
      model_name         TEXT,
      parameters_json    TEXT,
      rmse               DOUBLE,
      aic                DOUBLE,
      convergence_status TEXT
    )
  ")

  # ── dataset_conditions: pre-aggregated search summary (one row per condition) ───
  # Replaces the GROUP BY subquery on well_metadata in every search call.
  # Populated by qres_sync_conditions(); maintained by all import scripts.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dataset_conditions (
      dataset_id        TEXT NOT NULL,
      species           TEXT,
      strain            TEXT,
      medium_type       TEXT,
      perturbation_type TEXT,
      perturbation      TEXT,
      signal_type       TEXT
    )
  ")

  # ── Indexes on foreign-key columns (not covered by PRIMARY KEY) ───────────────
  # Speeds up JOIN chains: dataset→plate, plate→well_metadata, well_metadata→conditions.
  # Also helps species/perturbation prefix lookups in filtered queries.
  for (sql in c(
    "CREATE INDEX IF NOT EXISTS idx_wm_plate_id      ON well_metadata(plate_id)",
    "CREATE INDEX IF NOT EXISTS idx_plate_dataset_id ON plate(dataset_id)",
    "CREATE INDEX IF NOT EXISTS idx_dc_dataset_id    ON dataset_conditions(dataset_id)",
    "CREATE INDEX IF NOT EXISTS idx_dc_species       ON dataset_conditions(species)",
    "CREATE INDEX IF NOT EXISTS idx_dm_dataset_id    ON derived_metric(dataset_id)",
    "CREATE INDEX IF NOT EXISTS idx_dm_well_id       ON derived_metric(well_id)",
    "CREATE INDEX IF NOT EXISTS idx_mf_dataset_id    ON model_fit(dataset_id)",
    "CREATE INDEX IF NOT EXISTS idx_mf_well_id       ON model_fit(well_id)",
    "CREATE INDEX IF NOT EXISTS idx_pr_dataset_id    ON processing_run(dataset_id)"
  )) tryCatch(DBI::dbExecute(con, sql), error = function(e) NULL)

  # ── medium_composition: per-compound concentrations for chemically defined media ──
  # Populated by import scripts (e.g. Aida 2025 CDM dataset).
  # conc_mm      = concentration in this condition (mM)
  # conc_base_mm = concentration in the round's base pattern (mM) — for comparison
  # is_stressor  = TRUE for the compound being experimentally varied
  # is_buffer_adj= TRUE for pH-buffer compounds that co-vary with the stressor (e.g. KH2PO4)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS medium_composition (
      dataset_id    TEXT NOT NULL,
      compound      TEXT NOT NULL,
      conc_mm       DOUBLE,
      conc_base_mm  DOUBLE,
      is_stressor   BOOLEAN DEFAULT FALSE,
      is_buffer_adj BOOLEAN DEFAULT FALSE
    )
  ")

  # ── dose_response_registry: Parquet files for dose-response experiments ──────
  # Separate from raw_measurement_registry (which is time-series only).
  # Parquet schema: well, conc_ng_ml, value (blank-subtracted), value_raw.
  # signal_type / measurement_unit follow the same vocabulary as raw_measurement_registry.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dose_response_registry (
      dataset_id        TEXT NOT NULL,
      storage_type      TEXT DEFAULT 'parquet',
      path              TEXT,
      compound          TEXT,
      conc_unit         TEXT,
      signal_type       TEXT,
      measurement_unit  TEXT,
      n_wells           INTEGER,
      n_conc            INTEGER,
      created_at        TIMESTAMP DEFAULT current_timestamp
    )
  ")
  tryCatch(DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_drr_dataset_id ON dose_response_registry(dataset_id)"),
    error = function(e) NULL)

  # ── user_note: bug reports, observations, feature requests ───────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_note (
      note_id    TEXT PRIMARY KEY,
      dataset_id TEXT,
      note_type  TEXT,
      note_text  TEXT,
      author     TEXT,
      created_at TIMESTAMP DEFAULT current_timestamp
    )
  ")

  # ── upload_embargo: data access control for unpublished datasets ──────────
  # release_mode: 'immediate' | 'date' | 'biorxiv'
  # access_key:   32-char alphanumeric token shown once to uploader
  # passphrase_hash: SHA-256 of user passphrase (stored, never plain text)
  # released:     FALSE until mode condition is met; search filters hide FALSE rows
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS upload_embargo (
      dataset_id        TEXT PRIMARY KEY,
      uploader_email    TEXT NOT NULL,
      orcid             TEXT,
      access_key        TEXT NOT NULL,
      passphrase_hash   TEXT NOT NULL,
      release_mode      TEXT NOT NULL DEFAULT 'immediate',
      release_date      DATE,
      biorxiv_url       TEXT,
      released          BOOLEAN NOT NULL DEFAULT FALSE,
      needs_correction  BOOLEAN NOT NULL DEFAULT FALSE,
      created_at        TIMESTAMP DEFAULT current_timestamp,
      released_at       TIMESTAMP
    )
  ")

  # ── embargo_check_log: audit trail for bioRxiv polling ───────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS embargo_check_log (
      check_id     TEXT PRIMARY KEY,
      dataset_id   TEXT NOT NULL,
      checked_at   TIMESTAMP DEFAULT current_timestamp,
      result       TEXT,
      journal_doi  TEXT
    )
  ")

  invisible(TRUE)
}

# Safely add new columns to existing tables (forward migration helper)
qres_migrate_schema <- function(con) {
  tryCatch(DBI::dbExecute(con, "ALTER TABLE dataset ADD COLUMN IF NOT EXISTS notes TEXT"),
           error = function(e) NULL)
  tryCatch(DBI::dbExecute(con, "ALTER TABLE plate ADD COLUMN IF NOT EXISTS shake_freq_rpm DOUBLE"),
           error = function(e) NULL)
  tryCatch(DBI::dbExecute(con, "ALTER TABLE plate ADD COLUMN IF NOT EXISTS plate_colour TEXT"),
           error = function(e) NULL)
  tryCatch(DBI::dbExecute(con, "ALTER TABLE plate ADD COLUMN IF NOT EXISTS lid BOOLEAN"),
           error = function(e) NULL)
  tryCatch(DBI::dbExecute(con, "ALTER TABLE upload_embargo ADD COLUMN IF NOT EXISTS orcid TEXT"),
           error = function(e) NULL)
  tryCatch(DBI::dbExecute(con, "ALTER TABLE upload_embargo ADD COLUMN IF NOT EXISTS needs_correction BOOLEAN NOT NULL DEFAULT FALSE"),
           error = function(e) NULL)
  tryCatch(DBI::dbExecute(con, "ALTER TABLE processing_run ADD COLUMN IF NOT EXISTS container_hash TEXT"),
           error = function(e) NULL)
  tryCatch(DBI::dbExecute(con, "ALTER TABLE processing_run ADD COLUMN IF NOT EXISTS git_commit TEXT"),
           error = function(e) NULL)
  # Add measurement_unit to registry tables (added after initial release)
  tryCatch(DBI::dbExecute(con,
    "ALTER TABLE raw_measurement_registry ADD COLUMN IF NOT EXISTS measurement_unit TEXT"),
    error = function(e) NULL)
  tryCatch(DBI::dbExecute(con,
    "ALTER TABLE dose_response_registry ADD COLUMN IF NOT EXISTS measurement_unit TEXT"),
    error = function(e) NULL)
  # Auto-populate measurement_unit from signal_type for existing records
  for (upd in list(
    list("raw_measurement_registry", "OD600",        "OD"),
    list("raw_measurement_registry", "OD750",        "OD"),
    list("raw_measurement_registry", "fluorescence", "RFU"),
    list("raw_measurement_registry", "luminescence", "RLU"),
    list("raw_measurement_registry", "CFU",          "CFU/ml"),
    list("raw_measurement_registry", "cell_count",   "cells/ml"),
    list("dose_response_registry",   "fluorescence", "RFU"),
    list("dose_response_registry",   "luminescence", "RLU")
  )) tryCatch(DBI::dbExecute(con, sprintf(
    "UPDATE %s SET measurement_unit = '%s' WHERE signal_type = '%s' AND measurement_unit IS NULL",
    upd[[1]], upd[[3]], upd[[2]])), error = function(e) NULL)

  # Embargo tables (added in session 3 — safe to run on older DBs)
  qres_init_schema(con)   # CREATE TABLE IF NOT EXISTS is idempotent
  # Standardise Keio species name to match all other datasets and search filters
  tryCatch(DBI::dbExecute(con,
    "UPDATE well_metadata SET species = 'E. coli' WHERE species = 'Escherichia coli'"),
    error = function(e) NULL)
  # signal_type added to dataset_conditions (idempotent ADD COLUMN)
  tryCatch(DBI::dbExecute(con,
    "ALTER TABLE dataset_conditions ADD COLUMN IF NOT EXISTS signal_type TEXT"),
    error = function(e) NULL)
  # Back-fill signal_type for existing dataset_conditions rows that have it NULL
  tryCatch(DBI::dbExecute(con, "
    UPDATE dataset_conditions dc
    SET signal_type = COALESCE(
          (SELECT rmr.signal_type FROM raw_measurement_registry rmr WHERE rmr.dataset_id = dc.dataset_id LIMIT 1),
          (SELECT drr.signal_type FROM dose_response_registry   drr WHERE drr.dataset_id = dc.dataset_id LIMIT 1)
        )
    WHERE dc.signal_type IS NULL"), error = function(e) NULL)

  # Backfill dataset_conditions for any datasets not yet represented there.
  # The NOT IN guard makes this idempotent — only fills gaps, never duplicates.
  tryCatch(DBI::dbExecute(con, "
    INSERT INTO dataset_conditions
      (dataset_id, species, strain, medium_type, perturbation_type, perturbation, signal_type)
    SELECT DISTINCT p.dataset_id, w.species, w.strain,
           w.medium_type, w.perturbation_type, w.perturbation,
           COALESCE(rmr.signal_type, drr.signal_type)
    FROM well_metadata w
    JOIN plate p ON w.plate_id = p.plate_id
    LEFT JOIN raw_measurement_registry rmr ON rmr.dataset_id = p.dataset_id
    LEFT JOIN dose_response_registry   drr ON drr.dataset_id = p.dataset_id
    WHERE p.dataset_id NOT IN (
      SELECT DISTINCT dataset_id FROM dataset_conditions WHERE dataset_id IS NOT NULL
    )"), error = function(e) NULL)
  invisible(TRUE)
}

qres_reset_tables <- function(con) {
  tbls <- c('dataset_conditions', 'dose_response_registry', 'medium_composition',
            'user_note', 'model_fit', 'derived_metric',
            'processing_run', 'raw_measurement_registry', 'well_metadata', 'plate', 'dataset')
  for (t in tbls)
    tryCatch(DBI::dbExecute(con, paste('DROP TABLE IF EXISTS', t)), error = function(e) NULL)
  qres_init_schema(con)
  invisible(TRUE)
}
