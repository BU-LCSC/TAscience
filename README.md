# Trend analysis workflow

MODIS phenology trends (2001–2022) and climate attribution on BU SCC. 

## Setup

- Scripts on SCC: `/usr3/graduate/mkmoon/GitHub/TAscience/trend/`.
- Data and results root: `/projectnb/modislc/users/mkmoon/TAscience/trend/`. Paths below are relative to this directory. 
- Inputs: MODIS MCD12Q1-2 C6.1 under `/projectnb/modislc/projects/sat/data/mcd12q/`, GLDAS monthly NetCDFs in `data/climate/gldas/`, and SPEI NetCDFs in `data/climate/spei/`.
- Shapefiles: `data/shp/IPCC-WGI-reference-regions-v4.shp` and `data/shp/world-administrative-boundaries_edited.shp`, with their companion files.
- R packages: `terra`, `sf`, `Rcpp`, `RcppRoll`, `remotePARTS`, `RColorBrewer`, `scales`, `randomForest`, `fastshap`, `Ternary`; `lmodel2` for an optional statistics section in `000`.

Make R and these packages available before submitting jobs. Create missing output directories, especially `data/runLogs/`, `data/re/att/data/`, and `figures/att/att_rf_sh/`. Many outputs are overwritten on rerun.

## Script order

| Script | Purpose and main outputs |
| --- | --- |
| `001_ARreg.R` | Extract quality-filtered MODIS metrics for stable land cover and fit autoregressive trends; save coefficients/residuals in `data/re/vals/` and tile rasters in `data/re/rasters/`. |
| `002_map_merge.R` | Merge tile trends, standard deviations, and significance into `data/re/rasters/merge/`. |
| `003_map_trd.R` | Plot merged trend and variability maps in `figures/`. |
| `004_ht.R` → `005_ht_map.R` | Test regional trends, collect results in `data/re/parts/`, and map significance. |
| `006_climate_chg.R` | Calculate late-minus-early median climate differences; resample and save 15 layers in `data/re/rasters/climate/`. |
| `007_attribution.R` | Fit regional random forests/SHAP explanations and plot saved results; see below. |
| `000_basic_stats.R` | Optional checks of existing results; run after the relevant analyses. |

For attribution, the dependency chain is `001` → `002` → `006` → `007`. Regional testing (`004` → `005`) is a separate branch using `001` outputs.

Metric IDs are **1 = MGU (mid-greenup), 2 = MGD (mid-greendown), 3 = GSL, 4 = EVImax, 5 = EVIamp, 6 = EVIarea**. Regional loops use indices 1–44 from `unique(refReg$Acronym)`; preserve that shapefile ordering.

Subdirectories (`pre*`, `nbar`, `Python`) contain additional preparation/exploratory work; follow the top-level workflow above.

## Running jobs

Use `run_sh_script.R` as a collection of `qsub` examples. **Submit one stage at a time and wait/check outputs before the next.** Sourcing the whole file submits stages without dependency management.

| Launcher | Argument |
| --- | --- |
| `run_001.sh` | Three-digit tile + metric, e.g. `0011` (315 tiles). |
| `run_002.sh` | Metric ID, e.g. `1`. |
| `run_004.sh` | Three-digit metric + three-digit region, e.g. `001001`; prepare inputs first. |
| `run_006.sh` | Two-digit climate ID, `01`–`15`. |
| `run_007.sh` | Three-digit metric + three-digit region, e.g. `001001`; enable/isolate fitting first. |

Example climate job, using the existing launcher:

```bash
cd /projectnb/modislc/users/mkmoon/TAscience/trend/data/runLogs/
qsub -V -pe omp 4 -l h_rt=12:00:00 /usr3/graduate/mkmoon/GitHub/TAscience/trend/run_006.sh 11
```

The launchers use `R --vanilla < script.R ARG`, which matches the scripts' `commandArgs()` parsing. Interactively, set `tt`, `mm`, `rr`, or `vv` instead of running the argument-parsing block.
