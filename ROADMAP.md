# Features and Roadmap

## What you get per dataset

- **All datasets:** raw curves, source/DOI link, benchmarked metrics from every tool, comparison with your own upload and with other datasets.
- **Plate-reader data:** QC plots (CV over time, endpoint heatmap). Flask/tube data get a warning, since their well labels are virtual.
- **Author metrics (K, r, lag):** Keio, Aida, Kawano, Zhang.
- **Medium composition panel:** Aida (Kawano and Zhang: data stored, panel not yet shown).
- **Gene links (KEGG, UniProt):** Keio.
- **Evolution datasets:** Trajectory view (drug dose per transfer + OD) and Per-cycle view (each cycle as a growth curve, growth rate per cycle, CSV).

## Tool × metric

| Metric | model_free | gcplyr | growthrates | DGrowthR | KinBiont.jl |
|---|:---:|:---:|:---:|:---:|:---:|
| µmax (per capita) | ✅ | ✅ | ✅ | ✅ | ✅ |
| Lag time | — | ✅ | ✅ | — | ✅ |
| Doubling time | ✅ | — | — | — | — |
| AUC | ✅ | ✅ | — | ✅ | — |
| Max OD / ΔOD | ✅ | — | — | — | — |

## Next (priority order)

- [ ] Competition assays (Maier lab): data model, then importer (`experiment_type = 'competition'`).
- [ ] Endpoint view: dose–response curve + IC50/MIC (Waithaka puromycin first).
- [ ] Evolution: per-lineage resistance summary (fold change in dose), strain comparison.
- [ ] Upload format for user serial-transfer data.
- [ ] Medium-composition panel for Kawano and Zhang.
- [ ] Imports: Binsfeld 2025, MolE 2025, Brenzinger MIC data, yeast datasets.
- [ ] Normalised downloads in tiers: raw → blank-corrected → per-curve metrics → relative to control.
- [ ] Statistical tool comparison (agreement, bias, confidence intervals) instead of overlaid point estimates.
- [ ] Search by dataset_id; fuzzy species matching ("ecoli").
- [ ] Plugin result cache; responsive (mobile) layout.

Full to-do list and dev log: `notes_toDo.txt` (local, not in git).
