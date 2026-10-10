# PhaMiG — Architecture

## Overview

- **UI + server:** `app.R` (~6,400 lines), `shiny.router` pages: `/`, `search`, `analyse`, `metadata_prep`, `submit`, `ask`, `plugins`, `acknowledgement`.
- **Metadata, search, provenance:** DuckDB (`data/qres.duckdb`, or the `QRES_DB` env var).
- **Raw time series:** Parquet in `data/parquet/raw_measurements/` (dose–response in `data/parquet/dose_response/`). Paths in the DB are relative.
- **Startup:** UI renders first; then `qres_migrate_schema()` (idempotent) and the initial search run via `later::later()`.
- **Fitting:** plugins run in `future` workers (2) and are cached with `bindCache()`.

## Data flow

1. **Submit** → `generic_csv.R` / `logphase_reader.R` / `metadata.R` → DuckDB rows + Parquet → `qres_sync_conditions()`.
2. **Search** → rule parser → keyword search (`qres_search_datasets`) → LLM only if nothing hits. The LLM returns **filters only**; raw SQL from the LLM is never executed.
3. **Analyse** → `qres_get_raw_curves()` (reads `;`-separated multi-plate paths, normalises legacy layouts) → blank correction (`qres_prepare_curves`) → `qres_compare_plugin_outputs()` → plots.
4. **Evolution datasets** skip the batch pipeline: `qres_split_transfer_cycles()` turns each transfer cycle into its own curve.

Analyse modes: **A** = own upload only · **B** = repository only · **C** = both.

## Code modules (`R/`)

- `db/` — `connect.R`, `schema.R` (tables + migrations), `migrations.R`, `queries.R` (search, filters, raw curves).
- `import/` — generic readers (`generic_csv.R`, `logphase_reader.R`, `biotek.R`, `tecan.R`, `metadata.R`) and one batch importer per source: `keio`, `aida`, `zhang`, `kawano`, `shimoganadig`, `petrungaro`, `brenzinger`, `waithaka`, `tamminen`, `eisner`, `taylorcrooks`, `baker` (`*_import.R`, run manually; not sourced by the app).
- `plugins/` — `registry.R` (single entry point) + `model_free`, `gcplyr`, `growthrates`, `dgrothr`, `kinbiont` (Julia subprocess). `omniplate.R` is kept but not registered.
- `plots/` — `growth_curves.R`, `comparison.R`, `qc.R`.
- `llm/` — `rule_parser.R` (no API key), `nl_query.R` (ellmer: Anthropic, then local ollama), `schema_context.R` (prompt; one single-quoted string, so never put `'` inside it).
- `validation/` — `input_checks.R` (SQL-safe strings, raw CSV + metadata checks), `metadata_checks.R`, `plate_checks.R`.
- `embargo/` — `access_key.R` (register, delete, reopen), `release_check.R` (date + bioRxiv).
- `utils/` — `helpers.R` (well/time/replicate parsing; user text is never `eval`'d), `email.R` (blastula).
- `tests/regression_check.R` — 42 checks: static, DB integrity, analysis per source, search, evolution, server, security, delete/reopen, app start.

## Database tables

| Table | Holds |
|---|---|
| `dataset` | publication info, `experiment_type` (growth_curve · evolution · endpoint · competition) |
| `plate` | instrument, wavelength, temperature, plate format |
| `well_metadata` | per-well species, strain, medium, perturbation, blank/control flags |
| `dataset_conditions` | pre-aggregated search table (rebuilt by `qres_sync_conditions()`) |
| `raw_measurement_registry` | one row per dataset: Parquet path(s), signal type, n_wells |
| `dose_response_registry` | endpoint assays |
| `transfer_cycle` | evolution: well × cycle times, drug, dose, background OD |
| `medium_composition` | compound concentrations (mM, or `conc_value` + `conc_unit`) |
| `processing_run`, `derived_metric`, `model_fit` | tool runs and results (provenance) |
| `upload_embargo`, `embargo_check_log` | release mode, access key, passphrase hash, `needs_correction` |
| `user_note` | notes from users (shown escaped in the admin table) |

## Conventions

- `plate_id` = `{dataset_id}__plate_{N}`; `well_id` = `{plate_id}__{well}`.
- `signal_type`: `OD600` (also 595 nm; real wavelength in `plate.wavelength_nm`), `OD750`, `fluorescence`, `luminescence`, `CFU`, `cell_count`. Different signal types are never combined in one plot.
- `perturbation_type`: `chemical`, `genetic`, `temperature`, `evolution`, `none`.
- Blank wells: `species = ''` (the column is NOT NULL).
- Deleting a dataset: remove wells **by `plate_id`**, because `well_metadata.dataset_id` is NULL for older imports.
- Migrations: DuckDB cannot `ADD COLUMN … NOT NULL`, so add columns as nullable with a DEFAULT.
- Legacy: Keio and `public_*` well IDs use older patterns; they are internally consistent.
