QRES_SCHEMA_CONTEXT <- '
## PhaMiG (PHenotypic Analysis of Microbial Growth) — DuckDB schema

### Tables

**dataset** (one row per experiment file)
  dataset_id TEXT PK, study_id, title, doi, year INTEGER, authors,
  license, source_url, raw_file, time_unit

**plate** (one row per physical plate)
  plate_id TEXT PK, dataset_id FK, plate_format, plate_type,
  microplate_size, plate_reader, spectrophotometer,
  wavelength_nm DOUBLE, temperature_c DOUBLE, shake_freq DOUBLE, raw_file

**well_metadata** (one row per well)
  well_id TEXT PK, plate_id FK, well, species, taxon_id, strain,
  medium_state, medium_type, perturbation_type, perturbation,
  concentration DOUBLE, concentration_unit,
  is_blank BOOLEAN, is_control BOOLEAN, replicate INTEGER

**raw_measurement_registry** (one row per registered dataset)
  dataset_id TEXT PK, storage_type, path, n_rows BIGINT,
  n_wells INTEGER, signal_type, created_at TIMESTAMP

**derived_metric** (one row per metric per well per processing run)
  metric_id TEXT PK, processing_id, dataset_id, well_id,
  metric_name TEXT, metric_value DOUBLE, metric_unit, model_name

  metric_name values: mumax_percapita, lag_time_h, doubling_time_h,
  auc_trapezoid, max_od, delta_od, time_to_max_h, asymptote_od

**transfer_cycle** (serial-transfer / experimental-evolution datasets, perturbation_type = "evolution")
  dataset_id, well_id, cycle INTEGER, start_time_h, end_time_h, n_reads,
  drug TEXT, drug_conc DOUBLE, drug_conc_unit, co_treatment, background_od
  One row per well (evolving lineage) x growth cycle between transfers.

**processing_run** (one row per plugin execution)
  processing_id TEXT PK, dataset_id, tool_name, tool_version,
  correction_type, parameters_json, status, message, created_at

### Common join pattern
  FROM dataset d
  LEFT JOIN plate p USING(dataset_id)
  LEFT JOIN well_metadata w USING(plate_id)
  LEFT JOIN raw_measurement_registry rmr USING(dataset_id)

### Available R analysis functions (call instead of raw SQL when appropriate)
  qres_filter_datasets(con, species, temp_min, temp_max, medium) → dataset list
  qres_compare_plugin_outputs(raw_long, plugins) → metrics data frame
  qres_get_raw_curves(con, dataset_id) → long-format OD time series

  Available plugin names: model_free, gcplyr, growthrates, dgrothr, kinbiont
  dgrothr = DGrowthR (Gaussian Process Regression; needs remotes::install_github("bio-datascience/DGrowthR"))
  kinbiont = KinBiont.jl (needs JuliaCall + Julia)
'

QRES_EXAMPLE_QUERIES <- '
## Example natural language → SQL translations

Q: "Show all E. coli datasets between 30 and 37 degrees"
SQL:
  SELECT d.dataset_id, w.species, w.strain, p.temperature_c, w.medium_type
  FROM dataset d
  LEFT JOIN plate p USING(dataset_id)
  LEFT JOIN (SELECT plate_id, species, strain, medium_type FROM well_metadata GROUP BY 1,2,3,4) w USING(plate_id)
  WHERE lower(w.species) LIKE lower(\'%E. coli%\')
    AND p.temperature_c BETWEEN 30 AND 37
  ORDER BY p.temperature_c

Q: "What is the average max growth rate per temperature?"
SQL:
  SELECT ROUND(p.temperature_c,0) AS temp_c,
         AVG(dm.metric_value) AS mean_mumax,
         COUNT(*) AS n_wells
  FROM derived_metric dm
  JOIN processing_run pr USING(processing_id)
  JOIN dataset d ON dm.dataset_id = d.dataset_id
  JOIN plate p USING(dataset_id)
  WHERE dm.metric_name = \'mumax_percapita\'
  GROUP BY temp_c ORDER BY temp_c

Q: "List datasets with MOPS medium"
SQL:
  SELECT d.dataset_id, w.species, w.medium_type, p.temperature_c
  FROM dataset d
  LEFT JOIN plate p USING(dataset_id)
  LEFT JOIN (SELECT plate_id, species, medium_type FROM well_metadata GROUP BY 1,2,3) w USING(plate_id)
  WHERE lower(w.medium_type) LIKE \'%mops%\'

Q: "Compare gcplyr vs model_free mumax for 35-39°C"
SPECIAL: comparison — use qres_filter_datasets + qres_compare_plugin_outputs
  action: compare_tools
  species: null
  temp_min: 35, temp_max: 39
  medium: null
  plugins: ["gcplyr", "model_free"]
  metric: "mumax_percapita"
'
