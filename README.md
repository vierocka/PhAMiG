# PhaMiG — PHenotypic Analysis of Microbial Growth

Shiny + DuckDB repository of **raw** microbial growth data, with several analysis tools run on the same curves.

- **Goal:** find, inspect, compare and export growth curves; benchmark analysis tools on identical raw data.
- **Content (Oct 2026):** 12,965 datasets · 30 species · ~73,000 wells · 314,688 evolution transfer cycles → see [PUBLICATIONS.md](PUBLICATIONS.md).
- **Data types:** growth curves (12,952) · experimental evolution (11) · endpoint dose–response (2).
- **Policy:** raw measurements only; model-fitted or normalised curves are rejected.

---

## Run

```r
install.packages(c('shiny', 'shinythemes', 'shiny.router', 'shinycssloaders', 'DT', 'DBI', 'duckdb',
                   'arrow', 'ggplot2', 'dplyr', 'tidyr', 'data.table', 'readxl', 'future', 'promises',
                   'memoise', 'later', 'digest', 'openssl', 'blastula', 'ellmer', 'gcplyr', 'growthrates'))
shiny::runApp()
```

- `QRES_DB` — database path (default `data/qres.duckdb`).
- `ANTHROPIC_API_KEY` — optional LLM search fallback (a local ollama also works). Search works without it.
- `KINBIONT_SYSIMAGE` — optional KinBiont.jl speed-up (`julia julia/build_kinbiont_sysimage.jl`).
- Test after every change: `Rscript tests/regression_check.R --app`.

---

## Pages

- **Search** — one box (rule parser → keyword → LLM only if nothing hits); data-type cards; advanced filters; "Select all results"; CSV download.
- **Analyse** — growth curves + tool benchmarking; evolution datasets get a Trajectory / Per-cycle panel.
- **Metadata prep** — plate template generator and field reference.
- **Submit** — upload raw CSV/Excel + metadata; choose release mode; get a one-time access key.
- **Manage my data** — release status, delete, or "reopen" a dataset to upload a corrected version.
- **Plugins**, **Ask**, **Acknowledgements**.

---

## Analysis tools

| Tool | Language | Main metrics |
|---|---|---|
| model_free | R | doubling time, µmax, AUC, max OD |
| gcplyr | R | µmax, lag, AUC |
| growthrates | R | µmax, lag |
| DGrowthR | R (Gaussian process) | µmax, AUC |
| KinBiont.jl | Julia 1.11 (subprocess) | µmax, lag |

- All tools get identically pre-processed data (same blank subtraction).
- omniplate is not offered at the moment (needs a separate conda environment).

---

## Submitting data

- **Required:** `dataset_id`, `raw_file`, `species`, `medium_type`, `temperature_c`, `perturbation_type`.
- **perturbation_type:** `chemical` · `genetic` · `temperature` · `evolution` · `none`.
- **Raw file:** column 1 = time (h, min, s or HH:MM:SS); other columns = wells (`A1`…`H12`); template in `data/raw_csv/`.
- **Release modes:** immediate · on a date · when the bioRxiv preprint is published. Unreleased data are hidden everywhere.
- **Delete / reopen** needs email + 32-character access key + passphrase (SHA-256 hashed).

Details: [ARCHITECTURE.md](ARCHITECTURE.md) (code and data model), [ROADMAP.md](ROADMAP.md) (features and plans).
