# Data Sources

Datasets currently in the PhaMiG / Qres DB repository.
Numbers reflect the live database (last updated June 2026).

---

## Summary

| | Datasets | Growth curves | Species |
|---|---:|---:|---|
| **Total** | **8,872** | **37,892** | *E. coli*, *M. tuberculosis*, *T. brucei* |
| of which dose–response | **2** | — | *T. brucei* |

---

## Datasets by publication

### 1 — Lao & Ying (2026) · Keio single-gene knockout collection

| Field | Value |
|---|---|
| Authors | Lao Z, Ying BW |
| Year | 2026 |
| Journal | Scientific Data 13:717 |
| DOI | [10.1038/s41597-026-07075-9](https://doi.org/10.1038/s41597-026-07075-9) |
| Data | Figshare [28342043](https://doi.org/10.6084/m9.figshare.28342043) |
| Species | *E. coli* K-12 BW25113 |
| Strains | 3,909 single-gene knockouts (Keio collection) |
| Media | LB (rich) and M63 (minimal), both glucose-supplemented |
| Perturbation type | genetic |
| Temperature | 37 °C |
| Plate reader | — (microplate, OD600) |
| **Datasets** | **7,818** (3,909 genes × 2 media) |
| **Growth curves** | **23,454** (3 biological replicates per dataset) |
| Author metrics | Growth yield K and growth rate r (from paper, included as `derived_metric`) |
| Notes | Quality flags per curve: `indicator_K` (biological bias, replicate outlier) and `indicator_r` (computational bias, replicate outlier); gene functional category annotation from Table 2 of the paper; gene links to KEGG ECO and UniProt K-12 available in the app |

---

### 2 — Aida & Ying (2025) · Chemically defined media

| Field | Value |
|---|---|
| Authors | Aida H, Ying BW |
| Year | 2025 |
| Journal | Scientific Data 12:984 |
| DOI | [10.1038/s41597-025-05356-3](https://doi.org/10.1038/s41597-025-05356-3) |
| Data | Figshare [28342064](https://doi.org/10.6084/m9.figshare.28342064) |
| Species | *E. coli* BW25113 |
| Strain | Wild-type |
| Media | Chemically defined medium (CDM) with single-compound perturbations |
| Perturbation type | chemical (CDM composition variation) |
| Temperature | 37 °C |
| Design | 7 experimental rounds; each round tests 44 compounds × 4 concentration levels against a round-specific base medium; KH₂PO₄ co-varies as a pH-buffer adjuster |
| **Datasets** | **1,029** (one per unique medium condition) |
| **Growth curves** | **~13,448** (~13 biological replicates per condition) |
| Author metrics | Growth yield K and growth rate r included as `derived_metric` |
| Notes | Full per-compound concentrations stored in `medium_composition` table; the primary stressor and buffer adjuster flagged per condition; available in the app as the "Medium composition" panel on the dataset detail view |

---

### 3 — Katipoglu-Yazan et al. (2023) · Temperature gradient

| Field | Value |
|---|---|
| Authors | Katipoglu-Yazan T, Dev S, Desmond-Le Quemener E, Bouchez T |
| Year | 2023 |
| DOI | [10.57745/GCKG7W](https://doi.org/10.57745/GCKG7W) |
| Data | Zenodo (same DOI) |
| Species | *E. coli* K12 NCM3722 |
| Strain | K12 NCM3722 |
| Medium | MOPS + 3 g/L glucose |
| Perturbation type | temperature |
| Temperature range | 27–46 °C (1 °C steps) |
| Plate format | 48-well microplate, OD600, shaking 2.5 rpm |
| **Datasets** | **20** (one per temperature) |
| **Growth curves** | **960** (48 wells per plate) |
| Author metrics | — |
| Notes | Used for temperature-response benchmarking; QC plots available (plate-reader data with spatial well layout) |

---

### 4 — Sao Emani C et al. / Baker B (2018) · M. tuberculosis genetic complements

| Field | Value |
|---|---|
| Authors | Sao Emani C, Williams MJ, Van Helden PD, Taylor MJC, Carolis C, Wiid IJ, Baker B |
| Year | 2018 |
| Journal | Scientific Data 5:180184 |
| DOI | [10.1038/sdata.2018.184](https://doi.org/10.1038/sdata.2018.184) |
| Data | Scientific Data (supplement); knockout data in Metabolights MTBLS541 |
| Species | *Mycobacterium tuberculosis* |
| Strain | CDC1551 (wild-type) + ΔegtAc, ΔegtBc, ΔegtCc, ΔegtDc (ergothioneine pathway) + ΔmshAc (mycothiol pathway) |
| Medium | Middlebrook 7H9 + OADC |
| Perturbation type | genetic (complement strains) |
| Temperature | 37 °C, static, BSL-3 (double-sealed bags) |
| Measurement | Manual spectrophotometer, OD600, every 2 days over 24 days |
| **Datasets** | **1** |
| **Growth curves** | **18** (3 biological replicates × 6 strains) |
| Author metrics | — |
| Notes | **Phenotype-rescue validation dataset.** Complement strains restore the functional gene into the knockout background; growth should match wild-type CDC1551, confirming that any phenotype in the knockout (in Metabolights MTBLS541) is specifically caused by gene loss. QC warning shown: flask/tube data with artificial well labels, not a spatial microplate. |

---

---

### 5 — Waithaka et al. (2022) · *T. brucei* RBSR RNAi growth curves

| Field | Value |
|---|---|
| Authors | Waithaka BG, Ahrens M, Kishore S, Shankarampeta I, Kaufer A, Brecht M, Clayton CE |
| Year | 2022 |
| Journal | PLoS Neglected Tropical Diseases |
| DOI | [10.1371/journal.pntd.0010876](https://doi.org/10.1371/journal.pntd.0010876) |
| Species | *Trypanosoma brucei* |
| Strain | WT (parent) + RBSR1 and RBSR2 RNAi clones (c2, c3, c4 ± tetracycline) |
| Medium | HMI-9, 27 °C |
| Measurement | Hemocytometer cell counts, cumulative cells/mL |
| Signal type | `cell_count` (cumulative count without dilution bias) |
| **Datasets** | **2** (RBSR1 and RBSR2 knockdown lines) |
| **Growth curves** | **12** (5 + 7 conditions) |
| Notes | First *T. brucei* dataset in the repository. Cumulative cell counts track population growth across serial dilution passages. RBSR1 and RBSR2 are two RNA binding proteins of the RBSR family; RNAi knockdown is induced by tetracycline. |

---

### 6 — Waithaka et al. (2022) · *T. brucei* puromycin dose–response

| Field | Value |
|---|---|
| Authors | Waithaka BG, Ahrens M, Kishore S, Shankarampeta I, Kaufer A, Brecht M, Clayton CE |
| Year | 2022 |
| DOI | [10.1371/journal.pntd.0010876](https://doi.org/10.1371/journal.pntd.0010876) |
| Species | *Trypanosoma brucei* |
| Signal type | `fluorescence` (puromycin resistance reporter, blank-subtracted) |
| Experiment A | **Tethering candidates** — 9 mRNA processing factors (GFP ctrl, CPSF73, U2AF65, DRBD3, DRBD4, HNRNPFH, TRS1, TRSIP, RBSR) × 2 clones × ±tetracycline × 4 replicates |
| Experiment B | **PPT length reporters** — polypyrimidine tract (PPT) length variants × ±boxB tethering × replicates |
| X-axis | Puromycin concentration (0–12 800 ng/mL) |
| **Datasets** | **2** (tethering + PPT length) |
| Storage | `dose_response_registry` (separate from time-series `raw_measurement_registry`) |
| Notes | These are endpoint fluorescence assays — NOT time-series. Cannot be mixed with OD600 or cell count data. Stored in a dedicated `dose_response_registry` table and `data/parquet/dose_response/` directory. A dose–response analysis view (IC50 fitting) is planned but not yet in the UI. |

---

## Not yet integrated

| Publication | Species | Reason |
|---|---|---|
| Mustonen group (yeast) | *S. cerevisiae* | Integration in progress |
| Bollenbach group (yeast) | *S. cerevisiae* | Integration in progress |
| Holic group (yeast) | *S. cerevisiae* | Integration in progress |

---

## Citing

If you use data from this repository, please cite the original publications above.
The PhaMiG platform itself will be citable once the data descriptor manuscript is submitted.
