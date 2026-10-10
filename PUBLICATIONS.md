# Data Sources

Datasets in the PhaMiG database (`data/qres.duckdb`, updated 2026-10-10). Please cite the original publications.

| | Datasets | Wells / curves | Species |
|---|---:|---:|---:|
| **Total** | **12,965** | **~73,000** (+ 314,688 transfer cycles) | **30** |

---

## By publication

| # | Source | Organism | Design | Datasets | Curves |
|---|---|---|---|---:|---:|
| 1 | Lao & Ying 2026, *Sci Data* · [10.1038/s41597-026-07075-9](https://doi.org/10.1038/s41597-026-07075-9) | *E. coli* BW25113 | Keio knockouts × LB / M63, 37 °C | 7,818 | 23,454 |
| 2 | Zhang, Sawamura & Ying 2026, *Sci Data* · [10.1038/s41597-026-07990-x](https://doi.org/10.1038/s41597-026-07990-x) | 6 bacteria | species × defined medium | 2,543 | 11,686 |
| 3 | Kawano, Araki & Ying 2026 · Figshare [33716665](https://doi.org/10.6084/m9.figshare.33716665) | *E. coli* BW25113 | medium × antibiotic | 1,490 | 11,148 |
| 4 | Aida & Ying 2025, *Sci Data* · [10.1038/s41597-025-05356-3](https://doi.org/10.1038/s41597-025-05356-3) | *E. coli* BW25113 | 1,029 chemically defined media, 37 °C | 1,029 | 13,608 |
| 5 | Shimoga Nadig … Bollenbach 2025, *Open Biol* · [10.1098/rsob.240301](https://doi.org/10.1098/rsob.240301) | 11 strains | ± trace elements, M9 glucose, 30 °C | 21 | 3,360 |
| 6 | Petrungaro … Bollenbach 2026, *Nat Commun* · [10.1038/s41467-026-76025-1](https://doi.org/10.1038/s41467-026-76025-1) | *E. coli* (lab strains + UTI isolates) | morbidostat evolution (NIT, TMP, MEC) | 11 | 2,988 lineages |
| 7 | Brenzinger … Brochado 2024, *Nat Microbiol* · [10.1038/s41564-023-01556-y](https://doi.org/10.1038/s41564-023-01556-y) | *V. cholerae* | compound screen + checkerboards, 32 °C | 10 | 5,376 |
| 8 | Katipoglu-Yazan et al. 2023 · [10.57745/GCKG7W](https://doi.org/10.57745/GCKG7W) | *E. coli* NCM3722 | temperature 27–45 °C, MOPS glucose | 19 | 728 |
| 9 | Tamminen et al. 2020, *PLoS ONE* · [10.1371/journal.pone.0236822](https://doi.org/10.1371/journal.pone.0236822) | 8 marine fungi | carbon sources, 2 instruments, 24 °C | 16 | 400 |
| 10 | Waithaka et al. 2022, *PLoS NTD* · [10.1371/journal.pntd.0010876](https://doi.org/10.1371/journal.pntd.0010876) | *T. brucei* | RNAi cell counts + puromycin dose–response | 4 | 12 + endpoint |
| 11 | Eisner et al. 2022, *Front Microbiol* · [10.3389/fmicb.2022.817080](https://doi.org/10.3389/fmicb.2022.817080) | *M. xanthus* | rpoB strains × casitone, 4 plates, 32 °C | 1 | 190 |
| 12 | Kollerova et al. 2022 · Dryad [10.5061/dryad.rr4xgxdbj](https://doi.org/10.5061/dryad.rr4xgxdbj) | *E. coli* MG1655 | ampicillin 0–128 µg/ml, LB, 37 °C | 1 | 72 |
| 13 | Taylor & Crooks 2021, *Front Microbiol* · [10.3389/fmicb.2021.674835](https://doi.org/10.3389/fmicb.2021.674835) | *P. somerae* | gentamicin 0–100 µg/ml, anaerobic | 1 | 12 |
| 14 | Sao Emani … Baker 2018, *Sci Data* · [10.1038/sdata.2018.184](https://doi.org/10.1038/sdata.2018.184) | *M. tuberculosis* | complemented knockouts, flasks | 1 | 18 |

Curves = registered wells, including blank wells where the authors deposited them.

---

## Notes per source

- **Keio, Aida, Kawano, Zhang:** author K / r (/ lag) included as metrics. Keio and Aida curves were blank-subtracted by the authors; Zhang curves are raw.
- **Zhang:** temperature known only for assay II (32 °C); assays I and III are NULL until the article methods are published.
- **Kawano:** no article yet; temperature and instrument are undocumented. Concentrations in mM.
- **Petrungaro:** each well = one lineage; per-cycle drug dose in `transfer_cycle`. Open questions for the authors in `../Petrungaro_2026_resistance_evolution/questions_for_Gabi.md`.
- **Brenzinger:** checkerboards were background-subtracted by the authors; the parsed MIC data are not imported yet.
- **Waithaka:** cell counts (time series) and fluorescence dose–response (endpoint, `dose_response_registry`).
- **Baker, Taylor & Crooks:** flask/tube data; well labels are virtual (a QC warning is shown).

## Rejected

- Baig 2023 (curves min-max normalised, no time axis) · Maier 2018 (AUC only) · Brochado 2018 (fitness tables only) · Tabnelli 2024 (Gompertz-fitted curves).

## Planned

- Binsfeld 2025 (*PLOS Biol*) · MolE (Olayo-Alarcon 2025) · comBact (Cacace 2023, licence needed) · yeast datasets (Mustonen, Bollenbach, Holic groups).
