# Data Sources

Datasets currently in the PhaMiG / Qres DB repository.
Numbers reflect the live database (last updated June 2026).

---

## Summary

| | Datasets | Growth curves | Species |
|---|---:|---:|---|
| **Total** | **8,868** | **37,880** | *E. coli*, *M. tuberculosis* |

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
