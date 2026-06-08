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
  # dataset_id FK → dataset; path = Parquet file path
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS raw_measurement_registry (
      dataset_id   TEXT PRIMARY KEY,
      storage_type TEXT,
      path         TEXT,
      n_rows       BIGINT,
      n_wells      INTEGER,
      signal_type  TEXT,
      created_at   TIMESTAMP DEFAULT current_timestamp
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
      dataset_id       TEXT PRIMARY KEY,
      uploader_email   TEXT NOT NULL,
      access_key       TEXT NOT NULL,
      passphrase_hash  TEXT NOT NULL,
      release_mode     TEXT NOT NULL DEFAULT 'immediate',
      release_date     DATE,
      biorxiv_url      TEXT,
      released         BOOLEAN NOT NULL DEFAULT FALSE,
      created_at       TIMESTAMP DEFAULT current_timestamp,
      released_at      TIMESTAMP
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
  tryCatch(DBI::dbExecute(con, "ALTER TABLE processing_run ADD COLUMN IF NOT EXISTS container_hash TEXT"),
           error = function(e) NULL)
  # Embargo tables (added in session 3 — safe to run on older DBs)
  qres_init_schema(con)   # CREATE TABLE IF NOT EXISTS is idempotent
  invisible(TRUE)
}

qres_reset_tables <- function(con) {
  tbls <- c('user_note', 'model_fit', 'derived_metric', 'processing_run',
            'raw_measurement_registry', 'well_metadata', 'plate', 'dataset')
  for (t in tbls)
    tryCatch(DBI::dbExecute(con, paste('DROP TABLE IF EXISTS', t)), error = function(e) NULL)
  qres_init_schema(con)
  invisible(TRUE)
}
