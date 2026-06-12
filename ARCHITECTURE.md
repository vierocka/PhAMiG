# PhaMiG — Architecture Overview

How pages, data flows, and code modules connect.

---

## Page map and navigation

```
                    ┌─────────────────────────────────────┐
                    │             HOME  /                  │
                    │  ┌────────┐  ┌────────┐  ┌────────┐ │
                    │  │ Search │  │Analyse │  │  Ask   │ │
                    │  │ Data   │  │& Bench │  │  NL    │ │
                    │  └───┬────┘  └───┬────┘  └───┬────┘ │
                    │  ┌────────┐  ┌────────┐  ┌────────┐ │
                    │  │Submit  │  │Metadata│  │Plugins │ │
                    │  │ Data   │  │ Prep   │  │& Tools │ │
                    │  └───┬────┘  └────────┘  └────────┘ │
                    └──────┼──────────────────────────────┘
                           │ shiny.router (URL-hash routing)
```

Routing is handled by `shiny.router` — each tile links to a hash URL (`/search`,
`/analyse`, `/ask`, `/submit`, …). The router renders only the active page; the
server runs continuously and holds all reactive state.

---

## Cross-page data flow

```
  ┌──────────────────────────────────────────────────────────────┐
  │  SUBMIT DATA  /submit                                        │
  │                                                              │
  │  user uploads raw CSV/Excel + metaData.csv                   │
  │       │                                                      │
  │       ▼                                                      │
  │  import pipeline ──► DuckDB tables ──► Parquet files        │
  │  (generic_csv.R /     (dataset,          (data/parquet/      │
  │   logphase_reader.R)   plate,             raw_measurements/) │
  │                        well_metadata,                        │
  │                        raw_measurement_registry,             │
  │                        upload_embargo)                       │
  └──────────────────────────────────────────────────────────────┘
          │ persisted
          ▼
  ┌──────────────────────────────────────────────────────────────┐
  │  SEARCH & BROWSE  /search                                    │
  │                                                              │
  │  user filters by species / medium / temperature / …         │
  │  ──► qres_filter_datasets(con, …) ──► result table          │
  │                                             │                │
  │                              "Analyse selected" button       │
  │                                             │                │
  │                         analyze_selected_ids (reactiveVal)  │
  └─────────────────────────────────────────────┼───────────────┘
                                                │ change_page('analyse')
                                                ▼
  ┌──────────────────────────────────────────────────────────────┐
  │  ANALYSE & BENCHMARK  /analyse    (see detail below)        │
  └──────────────────────────────────────────────────────────────┘
          ▲
          │ nl_active_ids → change_page or same session
  ┌──────────────────────────────────────────────────────────────┐
  │  NATURAL LANGUAGE QUERY  /ask                                │
  │                                                              │
  │  user types free text                                        │
  │       │                                                      │
  │       ├─► rule_parser.R  (keyword/pattern matching)         │
  │       └─► nl_query.R     (Claude API, httr2)                │
  │                │                                             │
  │                ▼                                             │
  │          generated SQL ──► DuckDB ──► result table          │
  │          (schema_context.R provides table descriptions)     │
  │                                    │                         │
  │                          growth curves + tool benchmarking  │
  │                          (same pipeline as Analyse page)    │
  └──────────────────────────────────────────────────────────────┘
```

---

## Analyse & Benchmark — internal structure

```
  analyse_mode_rv  ('A' | 'B' | 'C')          analyze_selected_ids
  comp_upload_rv   (in-session upload data)     (from Search or NL page)
        │                                              │
        └──────────────┬───────────────────────────────┘
                       ▼
             comp_active_ids()   ← reactive: returns IDs based on mode
                       │
                       ▼
             comp_all_raw()      ← reads Parquet/CSV for active IDs;
                       │            binds upload (Mode A) + repo (Mode B/C)
                       │
          ┌────────────┴────────────────────────────────┐
          │  BLANK / CONTROL PIPELINE                    │
          │                                              │
          │  comp_blank_candidates()                     │
          │    SD < 0.1 per (dataset × well)             │
          │    → candidates list + "Add to blanks" btn   │
          │                                              │
          │  comp_upload_blanks  (selectize input)       │
          │  comp_upload_controls (selectize input)      │
          │                                              │
          │  qres_prepare_curves(raw, method,            │
          │    blank_names, blank_ids)                   │
          │    → .apply_baseline()  (per-dataset)        │
          │    → remove blank rows                       │
          └────────────┬─────────────────────────────────┘
                       │
          ┌────────────▼────────────────────────────────┐
          │  comp_curves_data()  ← cached reactive       │
          │    baseline-corrected, blanks removed,       │
          │    plate_label column attached               │
          └────────────┬─────────────────────────────────┘
                       │
          ┌────────────┴────────────────────────────────┐
          │                       │                      │
          ▼                       ▼                      ▼
  comp_curves              comp_metrics_long      comp_plot
  renderPlot               reactive               renderPlot
  (growth curves,          (runs all plugins      (box/scatter
   coloured by plate        via future_promise)    metric comparison)
   or dataset)             qres_compare_plugin_   qres_plot_metric_
  qres_plot_growth()        outputs()              comparison()
```

### Mode switching

```
  Mode A  ──  file upload only          (comp_upload_rv set, no DB lookup)
  Mode B  ──  repository datasets only  (reads from DuckDB + Parquet)
  Mode C  ──  upload + repository       (both sources bound together)

  Clear / navigate away  ──  wipes all reactiveVals, resets mode to NULL
```

---

## Code module map

```
app.R  (single server + UI file, ~5700 lines)
  │
  ├── sources on startup:
  │
  ├── R/db/
  │   ├── connect.R         open/close DuckDB connection (con)
  │   ├── schema.R          CREATE TABLE statements + table helpers
  │   ├── migrations.R      schema versioning (adds columns to existing DBs)
  │   └── queries.R         qres_filter_datasets, qres_get_raw_curves,
  │                         qres_get_blank_wells, qres_get_species_with_counts, …
  │
  ├── R/import/
  │   ├── generic_csv.R     CSV/Excel → long format → Parquet
  │   │                     (qres_raw_to_long, qres_excel_to_long,
  │   │                      qres_register_raw_measurements, .fix_excel_df)
  │   ├── logphase_reader.R LogPhase600 multi-plate Excel
  │   │                     (auto-reads well metadata from Results sheets)
  │   ├── metadata.R        metaData.csv → dataset/plate/well_metadata tables
  │   │                     calls qres_sync_conditions() after each batch
  │   ├── keio_import.R     Keio collection batch import (7,818 datasets)
  │   │                     Lao & Ying 2026 · DOI 10.1038/s41597-026-07075-9
  │   ├── aida_import.R     Aida CDM batch import (1,029 datasets)
  │   │                     Aida & Ying 2025 · DOI 10.1038/s41597-025-05356-3
  │   │                     also writes medium_composition table
  │   ├── baker_import.R    Baker 2018 M. tuberculosis import (1 dataset)
  │   │                     Sao Emani et al. 2018 · DOI 10.1038/sdata.2018.184
  │   ├── waithaka_import.R Waithaka 2022 T. brucei import (2+2 datasets)
  │   │                     DOI 10.1371/journal.pntd.0010876
  │   │                     · waithaka_import_rbsr()    → cell_count time-series (raw_measurement_registry)
  │   │                     · waithaka_import_puro_dr() → fluorescence dose-response (dose_response_registry)
  │   ├── taylorcrooks_import.R Taylor & Crooks 2021 P. somerae KFU gentamicin (1 dataset)
  │   │                     DOI 10.3389/fmicb.2021.674835
  │   ├── tamminen_import.R Tamminen 2020 marine fungi import (16 datasets)
  │   │                     DOI 10.1371/journal.pone.0236822
  │   │                     · tamminen_import_bioscreen() → BioScreen C MBR (OD600, 30 min)
  │   │                     · tamminen_import_cytomat()   → Cytomat+DTX 880 (OD595, ~2 h)
  │   ├── eisner_import.R   Eisner 2022 M. xanthus rpoB growth (1 dataset, 4 bio-rep plates)
  │   │                     DOI 10.3389/fmicb.2022.817080
  │   │                     · eisner_import_growth()    → 4 plates, TECAN M200 PRO, OD595
  │   ├── biotek.R          BioTek raw format reader
  │   └── tecan.R           TECAN raw format reader
  │
  ├── R/plugins/
  │   ├── registry.R        qres_compare_plugin_outputs()  ← single entry point
  │   │                     dispatches to all enabled plugins, binds results
  │   ├── model_free.R      log-linear doubling time, AUC, max OD
  │   ├── gcplyr.R          gcplyr wrapper
  │   ├── growthrates.R     growthrates wrapper
  │   ├── dgrothr.R         DGrowthR (Gaussian Process Regression)
  │   ├── kinbiont.R        KinBiont.jl via JuliaCall
  │   └── omniplate.R       omniplate via reticulate (Python)
  │
  ├── R/plots/
  │   ├── growth_curves.R   .apply_baseline(), qres_prepare_curves(),
  │   │                     qres_plot_growth()
  │   ├── comparison.R      qres_plot_metric_comparison() (box + scatter)
  │   └── qc.R              QC / diagnostic plots
  │
  ├── R/llm/
  │   ├── nl_query.R        Claude API call (httr2), response validation
  │   ├── rule_parser.R     keyword / slot-filling NL → SQL (no API key needed)
  │   └── schema_context.R  DB schema description injected into LLM system prompt
  │
  ├── R/validation/
  │   ├── input_checks.R    qres_sql_safe, qres_validate_raw_csv
  │   ├── metadata_checks.R metaData.csv column/type validation
  │   └── plate_checks.R    per-plate consistency checks
  │
  ├── R/embargo/
  │   ├── access_key.R      32-char key generation, 3-factor verification
  │   └── release_check.R   date-based + bioRxiv DOI polling
  │
  └── R/utils/
      ├── helpers.R         .parse_wells(), .parse_time_to_hours(),
      │                     .classify_well_label(), .expand_well_token()
      └── email.R           submission confirmation email (SMTP via blastula)
```

---

## Reactive state — shared values

```
  reactiveVal             set by                    read by
  ─────────────────────   ───────────────────────   ──────────────────────────
  analyze_selected_ids    Search page,              comp_active_ids(),
                          NL page, Search-in-       analyse_mode_rv observer
                          Analyse table

  analyse_mode_rv         mode card click,          comp_active_ids(), all
                          Search "Analyse" btn,      comp_* reactives, UI
                          Clear btn, navigate-away   conditionalPanels

  comp_upload_rv          file upload observer      comp_all_raw(),
                                                    comp_curves_data(),
                                                    comp_blank_candidates()

  comp_upload_n_plates    upload observer           comp_curves renderPlot,
                                                    comp_is_multiplate output

  comp_plate_labels_rv    upload observer           comp_curves_data()
                          (plate_id → sheet name)   (plate_label column)

  comp_well_meta_rv       upload observer           well meta table,
                          (LogPhase600 Results)      blank pre-selection

  comp_replicates_rv      replicate UI + meta       comp_metrics_long,
                          (well → group ID)          qres_plot_growth rep_groups
```

---

## Data storage layout

```
data/
├── parquet/
│   └── raw_measurements/
│       └── <dataset_id>.parquet    ← columnar time-series (fast reads)
├── long_csv/
│   └── raw_measurements/           ← fallback if arrow not available
│       └── <dataset_id>_long.csv
└── raw_csv/
    └── raw_data_template.csv       ← template for users

qres.duckdb                         ← all metadata, registry, embargo
  ├── dataset                       (id, title, authors, year, doi, raw_file)
  ├── plate                         (id, dataset_id, temperature_c, plate_reader)
  ├── well_metadata                 (well, plate_id, species, strain, medium,
  │                                  perturbation, is_blank, is_control)
  ├── dataset_conditions            (dataset_id, species, strain, medium_type,
  │                                  perturbation_type, perturbation)
  │                                 ← pre-aggregated GROUP BY result; replaces
  │                                   the per-query subquery on well_metadata.
  │                                   Populated by qres_sync_conditions() after
  │                                   every import. Indexed on dataset_id + species.
  ├── medium_composition            (dataset_id, compound, conc_mm, conc_base_mm,
  │                                  is_stressor, is_buffer_adj)
  │                                 ← populated by Aida CDM import only
  ├── raw_measurement_registry      (dataset_id, path, storage_type, n_wells)
  │                                ← time-series only (OD600, cell_count, etc.)
  ├── dose_response_registry        (dataset_id, path, compound, conc_unit, signal_type,
  │                                  n_wells, n_conc)
  │                                ← endpoint fluorescence / viability assays
  │                                  Parquet: well, conc_ng_ml, value, value_raw
  │                                  stored in data/parquet/dose_response/
  ├── processing_run                (run_id, dataset_id, plugin, timestamp)
  ├── derived_metric                (run_id, well_id, metric_name, value)
  └── upload_embargo                (dataset_id, email_hash, key_hash,
                                     passphrase_hash, release_mode, released)
```

---

## Blank / baseline pipeline (shared across all modes)

```
  User input (any mode)
       │
       ├── manual text entry  →  .parse_wells()  →  well names
       ├── auto-detect        →  comp_blank_candidates()
       │                          or nl_blank_candidates()
       │                          SD < 0.1 per (dataset × well)
       └── DB blanks (is_blank=TRUE)  →  qres_get_blank_wells(con, ids)
                                                         │
                                    union of all blank sources
                                                         │
                                      qres_prepare_curves(raw,
                                        method, blank_names, blank_ids)
                                                │
                                   .apply_baseline()   ← per-dataset when
                                                │         multiple datasets
                                   remove blank rows from data frame
                                                │
                                   qres_plot_growth()  /  qres_compare_plugin_outputs()
```

---

## NL Query pipeline

```
  User types free text
        │
        ├─► rule_parser.R
        │     slot-fill: species / temperature / medium / perturbation
        │     → SQL WHERE clauses (no API key required)
        │
        └─► nl_query.R  (if rule parser yields no results or user uses Ask)
              │  system prompt = schema_context.R (table + column descriptions)
              │  user prompt   = free text
              ▼
              Claude API  (claude-sonnet or claude-haiku)
              │
              ▼
              generated SQL  →  DuckDB  →  result data.frame
                                                │
                                         nl_result() reactive
                                                │
                              ┌─────────────────┴─────────────┐
                              ▼                               ▼
                        Results table                 Growth curves tab
                        (DTOutput)                    nl_curves_plot
                                                      + Tool benchmarking tab
                                                      nl_comp_metrics_long
                                                      → same plugin pipeline
                                                        as Analyse page
```

---

## Async fitting

Long-running plugin computations use `future_promise` to avoid blocking the UI:

```
  comp_metrics_long  reactive
        │
        └─► future({ qres_compare_plugin_outputs(raw, plugins) })
                              │
                  runs in separate R process (plan multisession, 2 workers)
                              │
                  %...>% (promise pipe)  →  result stored, plot redraws
```

Results are cached with `bindCache(comp_all_raw(), ...)` keyed on the raw data
and selected tools, so switching tabs or changing unrelated inputs does not
re-trigger fitting.

---

## Naming conventions

### Key identifiers

| Identifier | Pattern | Example |
|---|---|---|
| `dataset_id` | `{author}_{year}_{organism_abbrev}_{context}` | `eisner_2022_mx_rpob` |
| `plate_id` | `{dataset_id}__plate_{N}` | `eisner_2022_mx_rpob__plate_1` |
| `well_id` | `{plate_id}__{well}` | `eisner_2022_mx_rpob__plate_1__A1` |
| `well` | plate-reader well label or virtual name | `A1`, `ctrl_r1`, `R1` |

The double-underscore `__` separator is chosen so that `plate_id` can be extracted from `well_id` by splitting on `__` and taking all but the last token.

### Signal type vocabulary

| `signal_type` | Physical measurement | `measurement_unit` |
|---|---|---|
| `OD600` | Optical density (approx. 600 nm, also used for 595 nm) | `OD` |
| `OD750` | Optical density at 750 nm | `OD` |
| `fluorescence` | Fluorescence intensity | `RFU` |
| `luminescence` | Luminescence | `RLU` |
| `CFU` | Colony-forming units | `CFU/ml` |
| `cell_count` | Direct cell count (hemocytometer, flow cytometry) | `cells/ml` |

**Rule:** datasets with different `signal_type` values cannot be combined in the same analysis — the y-axes are dimensionally incompatible.

### Known naming inconsistencies (legacy, not yet fixed)

| File | Issue |
|---|---|
| `generic_csv.R`, `logphase_reader.R` | `well_id = paste(dataset_id, plate_id, well, sep='__')` — redundantly includes `dataset_id` prefix |
| `keio_import.R` (batch path) | Some `well_id` values built as `dataset_id__plate_1__R{rep_n}` instead of `plate_id__well` |

These were imported before the convention was formalised. Fixing them would require rewriting all affected Parquet files and DB records. The inconsistency is contained (each dataset's IDs are still internally consistent) and does not affect query correctness.

---

## Startup sequence

```
  session starts
       │
       ├─ session$onFlushed(once=TRUE)
       │       │
       │       └─ later::later(delay=0)   ← deferred so the browser receives
       │               │                    the initial HTML before DB queries run
       │               ├─ qres_migrate_schema(con)    (DDL + backfill)
       │               ├─ qres_search_datasets(con)   → dataset_results reactiveVal
       │               └─ updateSelectizeInput × 3    (species, author, perturb_type)
       │
       └─ UI renders immediately; search results populate ~1–3 s later
```

`qres_migrate_schema` is idempotent: it runs `ALTER TABLE … ADD COLUMN IF NOT EXISTS`,
creates missing tables, backfills `dataset_conditions` for any datasets not yet
represented there, and standardises species names.  Safe to run on every startup.
