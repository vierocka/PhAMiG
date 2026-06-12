# What you can get from a dataset

This page describes what the app can show for a dataset, what depends on the data
type, and what is planned but not yet implemented.

---

## Feature availability by dataset

When you select a dataset in Search & Browse, a right-hand detail panel appears.
What is shown depends on what was deposited.

| Feature | Katipoglu-Yazan (temp) | Keio (gene KO) | Aida (CDM) | Baker (M.tb) |
|---|:---:|:---:|:---:|:---:|
| Raw growth curves | ✅ | ✅ | ✅ | ✅ |
| DOI / data repository link | ✅ | ✅ | ✅ | ✅ |
| QC: CV over time plot | ✅ | ✅ | ✅ | ⚠️ |
| QC: endpoint heatmap | ✅ | ✅ | ✅ | ⚠️ |
| Medium composition panel | — | — | ✅ | — |
| Gene links (KEGG, UniProt) | — | ✅ | — | — |
| Author-provided metrics | — | ✅ K, r | ✅ K, r | — |
| Benchmarked metrics (5 tools) | ✅ | ✅ | ✅ | ✅ |
| Comparison to own upload | ✅ | ✅ | ✅ | ✅ |
| Comparison to other DB datasets | ✅ | ✅ | ✅ | ✅ |

**⚠️ Baker**: data were measured in 75 cm² flasks, not a microplate. Well labels (A1–F3)
are assigned per strain group. The CV plot reflects between-strain variation, not
technical replicate agreement. A warning is shown; the plots are still rendered.

---

## Benchmarked metrics — tool × metric matrix

Not all tools compute the same metrics. The table below shows what each enabled
tool returns and what the output column is called.

| Metric | model_free | gcplyr | growthrates | DGrowthR | KinBiont.jl |
|---|:---:|:---:|:---:|:---:|:---:|
| Max growth rate µmax (per-capita) | ✅ | ✅ | ✅ | ✅ | ✅ |
| Lag time | — | ✅ | ✅ | — | ✅ |
| Doubling time (log-linear) | ✅ | — | — | — | — |
| AUC (area under curve) | ✅ | ✅ | — | ✅ | — |
| Max OD (carrying capacity proxy) | ✅ | — | — | — | — |
| Delta OD (max − t0) | ✅ | — | — | — | — |
| Absolute µmax (OD/h) | — | — | — | — | — |

> **omniplate** (Python) produces absolute µmax and AUC but is not yet offered
> in the current deployment (requires a conda environment).

> **DGrowthR** uses Gaussian Process Regression; installed but may require
> `remotes::install_github('bio-datascience/DGrowthR')`.

> **KinBiont.jl** requires Julia 1.11 and a pre-compiled sysimage (`KINBIONT_SYSIMAGE`
> in `~/.Renviron`).

All tools receive identically pre-processed data (same blank subtraction,
same baseline method) so comparisons are on equal footing.

---

## QC plots — when they are shown

QC plots require plate-reader data with standard well coordinates (A1–H12).

| Data type | QC shown | Notes |
|---|---|---|
| 96-well microplate | ✅ Both plots | Standard layout |
| 48-well microplate | ✅ Both plots | Rows A–F, cols 1–8 |
| Flask / tube data, well labels present | ⚠️ With warning | Labels are artificial; CV reflects strain variation, not replicate error |
| Non-plate data, no well labels | ℹ️ Info message | Data deposited without spatial plate structure |

---

## What is planned (roadmap)

Items below are not yet implemented. They are roughly ordered by priority.

### Data coverage
- [x] *T. brucei* RBSR RNAi growth curves — Waithaka 2022, `signal_type = 'cell_count'`
- [x] *T. brucei* puromycin dose–response — Waithaka 2022, `signal_type = 'fluorescence'`
- [ ] *S. cerevisiae* datasets (Mustonen, Bollenbach, Holic groups)
- [ ] Additional *M. tuberculosis* knockouts (Metabolights MTBLS541)
- [ ] Additional perturbation types: pH, osmotic stress, nutrient limitation

### Analysis
- [ ] omniplate Python plugin (absolute µmax, AUC)
- [ ] Dose–response analysis view: IC50/MIC fitting for datasets in `dose_response_registry` (Waithaka puro data ready)
- [ ] Growth rate vs. temperature (Eppley curve, Arrhenius fit) for the NCM3722 temperature gradient data
- [ ] Replicate aggregation: mean ± SD envelope option in growth curve plots

### Search & discovery
- [ ] Full-text search across dataset notes and titles
- [ ] Download selected datasets as a single ZIP (Parquet + metadata CSV)
- [ ] Filter by author-provided metric range (e.g., "µmax > 0.5 h⁻¹")

### Benchmarking
- [ ] Cross-dataset metric comparison: how consistent is tool X across datasets from different labs?
- [ ] Reproducibility score: compare metric estimates from replicate experiments deposited by different groups

### Infrastructure
- [ ] True async DB queries with `future` + `promises` (currently deferred with `later::later` but still single-threaded)
- [ ] Multi-user write support (DuckDB WAL mode or PostgreSQL backend)
- [ ] Export benchmarking results as publication-ready CSV / R data frame
