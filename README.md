# PhaMiG — PHenotypic Analysis of Microbial Growth

A Shiny + DuckDB database for microbial growth curve storage, multi-tool analysis, and cross-study comparison.
Covers bacteria and yeast. Built for antibiotic resistance research and benchmarking of growth curve analysis tools.

**Scientific value proposition:** Run gcplyr, growthrates, DGrowthR, KinBiont.jl, omniplate and a model-free
estimator on the *same* raw data, with all results queryable by species / medium / temperature / perturbation.

---

## Metadata structure

Every dataset in PhaMiG requires two paired files linked by `dataset_id`:

| File | Content | Key column |
|------|---------|------------|
| `metaData.csv` | Dataset-level metadata (one row per raw CSV) | `dataset_id` |
| Raw plate reader CSV | Time-series OD measurements | — |
| Plate template CSV *(optional)* | Per-well condition breakdown | `well` |

### metaData.csv — required columns ★

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `dataset_id` ★ | text | **Unique key** — links metadata to raw data and all derived results | `ecoli_LB_37C_2024_smith` |
| `raw_file` ★ | text | Filename of the raw CSV (time + OD values) | `plate01_run3.csv` |
| `species` ★ | text | Organism name | `Escherichia coli` |
| `medium_type` ★ | text | Growth medium | `MOPS + 3 g/L glucose` |
| `temperature_c` ★ | number | Measurement temperature in °C | `37` |
| `perturbation_type` ★ | text | `chemical` \| `genetic` \| `temperature` \| `none` | `chemical` |

### metaData.csv — optional columns

| Column | Description | Example |
|--------|-------------|---------|
| `perturbation` | Perturbation agent or value | `rifampicin` / `37` / `ΔtrpB` |
| `strain` | Strain designation | `K12 NCM3722` |
| `taxon_id` | NCBI taxonomy ID | `562` |
| `medium_state` | `liquid` or `solid` | `liquid` |
| `concentration` | Perturbation amount (number) | `50` |
| `concentration_unit` | Unit for concentration | `mg/L` |
| `plate_format` | Number of wells | `96` |
| `plate_reader` | Instrument name/model | `Biotek Synergy H1` |
| `wavelength_nm` | Measurement wavelength | `600` |
| `shake_freq_rpm` | Orbital shaking frequency | `150` |
| `spectrophotometer` | Spectrophotometer model | `OD600` |
| `time_unit` | `h` (hours) \| `min` \| `s` | `h` |
| `authors` | Publication authors | `Smith, J.; Doe, A.` |
| `year` | Publication year | `2024` |
| `doi` | Digital object identifier | `10.1234/...` |
| `notes` | Free-text notes | — |

### Raw data CSV format

```
time_h, A1,   A2,   A3,   ...
0.0,    0.05, 0.05, 0.06,
0.5,    0.06, 0.05, 0.07,
...
```

- Column 1: time in the unit specified by `time_unit` (default: hours)
- Columns 2–N: OD values; header = well names (`A1`, `A2`, …, `H12`)

### Plate template CSV (from Metadata Preparation page)

One row per well. Columns match `well_metadata` table fields:
`well, species, strain, medium_type, perturbation_type, perturbation, concentration, concentration_unit, is_blank, is_control`

Generated automatically by the **Metadata Preparation** page in the app.

### Unique key chain

```
dataset_id (user-assigned, unique)
  └── dataset table       (one row: publication info, raw_file, time_unit)
  └── plate table         (one row: instrument, temperature_c, plate_reader…)
  └── well_metadata       (N rows: per-well biology — species, medium, perturbation…)
  └── raw_measurement_registry (one row: Parquet path, n_wells, n_rows)
  └── processing_run      (one row per plugin execution)
  └── derived_metric      (one row per metric per well per run)
```

---

## Searchable fields

All fields below are indexed in DuckDB and exposed in the Search UI:

| UI label | DB column | Table |
|----------|-----------|-------|
| Species | `well_metadata.species` | well_metadata |
| Strain | `well_metadata.strain` | well_metadata |
| Medium type | `well_metadata.medium_type` | well_metadata |
| Perturbation type | `well_metadata.perturbation_type` | well_metadata |
| Perturbation | `well_metadata.perturbation` | well_metadata |
| Temperature (°C) | `plate.temperature_c` | plate |
| Authors | `dataset.authors` | dataset |
| Year | `dataset.year` | dataset |
| DOI | `dataset.doi` | dataset |
| Plate reader | `plate.plate_reader` | plate |

**Temperature-as-perturbation search:** when `perturbation_type = temperature` and you enter a number in
"Perturbation name", the search matches both `well_metadata.perturbation` and `plate.temperature_c ± 0.5°C`.

---

## Analysis plugins

| Plugin | Language | Key metric | Status |
|--------|----------|------------|--------|
| model_free | R | doubling_time_h, mumax_percapita, AUC | ✅ Working |
| gcplyr | R | mumax_percapita, lag_time_h | ✅ Working |
| growthrates | R | mumax_percapita, lag_time_h | ✅ Working |
| DGrowthR | R (GPR) | mumax_percapita, AUC | ✅ Installed |
| KinBiont.jl | Julia | mumax_percapita, lag_time_h | ⚠️ Needs Julia 1.11 (CHOLMOD compat) |
| omniplate | Python | mumax_abs_od_per_h, AUC | ✅ Wired via reticulate |

**All plugins receive identically pre-processed data** (blank subtraction, optional smoothing)
to make cross-tool comparisons honest.

---

## Stack

- **Shiny** + `shiny.router` (v0.3.x, cerulean theme) — user interface
- **DuckDB** — metadata, registry, provenance, search, derived metrics
- **Arrow / Parquet** — fast columnar storage for raw time-series data
- **duckplyr** — DuckDB-accelerated dplyr operations
- **reticulate** — Python bridge for omniplate (`conda env: omniplate_env`, Python 3.11)
- **JuliaCall** — Julia bridge for KinBiont.jl
- **httr2** — Anthropic Claude API for natural-language query fallback

---

## Install & run

```r
install.packages(c(
  'shiny', 'shinythemes', 'shiny.router',
  'DT', 'DBI', 'duckdb', 'duckplyr',
  'ggplot2', 'data.table', 'readr', 'dplyr', 'tidyr',
  'glue', 'jsonlite', 'arrow', 'httr2', 'reticulate'
))

# Growth analysis plugins
install.packages(c('gcplyr', 'growthrates'))
# remotes::install_github('bio-datascience/DGrowthR')  # DGrowthR

# Python bridge (omniplate)
# conda create -n omniplate_env python=3.11
# conda run -n omniplate_env pip install omniplate

shiny::runApp()
```

Set `ANTHROPIC_API_KEY` for natural-language search (optional — rule-based NL search works without it):

```r
Sys.setenv(ANTHROPIC_API_KEY = 'sk-ant-...')
```

---

## Data access and embargo

PhaMiG supports three release modes for uploaded datasets:

| Mode | Behaviour |
|------|-----------|
| **Immediate** | Dataset is publicly searchable as soon as it is registered (use for published data) |
| **Set a release date** | Dataset is hidden until the chosen date; auto-released at midnight |
| **bioRxiv preprint** | Dataset is hidden until the bioRxiv preprint gains a journal DOI (monthly API check) |

Unreleased datasets are **completely hidden** from all search results, analysis tabs, and downloads.

### Deletion key chain

Every upload generates a **32-character access key** (shown once). To delete a dataset, provide:

1. Your **email address** (provided at upload, case-insensitive)
2. The **32-character access key** (random, not recoverable — save it!)
3. Your **passphrase** (≥8 characters, hashed with SHA-256 before storage)

All three must match. Deletion removes raw data, metadata, and all analysis results permanently.

Manage your datasets on the **Manage my data** tab of the Submit page.

---

## App pages

| Page | Route | Description |
|------|-------|-------------|
| Home | `/` | 6-pictogram navigation grid |
| Metadata Preparation | `/metadata_prep` | Plate template generator with required/optional fields, field reference |
| Search & Browse | `/search` | Keyword + NL + perturbation-type filtering; download results |
| Analyse & Compare | `/analyse` | Multi-dataset, multi-tool comparison; box plots, scatter, curves |
| Contact & NL Query | `/ask` | Free-text natural language queries (rule-based + Claude fallback) |
| Submit Data | `/submit` | Upload raw CSV + metadata; choose release mode; view access key |
| Manage my data | `/submit` (tab) | Delete dataset by 3-factor auth; check release status by email |
| Plugins & Tools | `/plugins` | Plugin status, references, processing log |
| Acknowledgements | `/acknowledgement` | Funding, tool authors, similar resources |

---

## Current datasets

19 *E. coli* K12 NCM3722 growth curves in MOPS + 3 g/L glucose, 27–45 °C (1°C steps).
Source: Katipoglu-Yazan et al. (2023). DOI: [10.57745/GCKG7W](https://doi.org/10.57745/GCKG7W)

Yeast datasets (Ville Mustonen, Tobias Bollenbach, Roman Holic groups) — integration in progress.
