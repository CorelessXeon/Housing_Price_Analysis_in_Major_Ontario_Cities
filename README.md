# Housing Price Analysis in Major Ontario Cities

This project builds a city-by-month panel for six Ontario CMAs from `2016-01` to `2024-12` and fits a Bayesian multilevel model for `log(price_index)`.

## Local data choices

- Housing prices: `data/data_raw/Housing_price_index.csv` (Statistics Canada NHPI).
- Policy rate: `data/data_raw/Interest_rate.csv`, which contains the daily overnight-rate target series and is aggregated to monthly means.
- International student enrolment: `data/data_raw/International_students.csv` (StatCan table 37-10-0232-01 — "Postsecondary enrolments by institution"). Annual count of international students enrolled per CMA, covering 2015/2016–2023/2024. Suppressed values (`..`) are imputed: leading years before an institution existed are set to 0; interior gaps are filled with the institution's mean across available years.
- Housing starts: `data/data_raw/New_houses_built.csv`, which now contains the six target city geographies directly and is merged at the city-month level.

## Project structure

- `data/data_raw/`: original local source files.
- `data/clean/`: cleaned source tables and the merged panel dataset.
- `output/model/`: fitted model object and text summaries.
- `output/tables/`: posterior summary tables and panel missingness.
- `output/figures/`: trace plots, posterior predictive checks, and city-effect plots.
- `output/notes/`: source and harmonization notes.

## Scripts

- `01_inspect_files.R`: inventory the local files and summarize which sources are usable.
- `02_clean_each_source.R`: clean each source and save intermediate datasets.
- `03_build_panel.R`: merge the cleaned sources into a city-by-month panel.
- `04_fit_bayesian_model.R`: fit the Bayesian multilevel model in `brms` with random intercepts by city and a random slope for housing supply by city.
- `05_diagnostics_and_outputs.R`: create posterior summaries, convergence diagnostics, and posterior predictive checks.

## Run order

Run the scripts from the project root in this order:

```r
source("01_inspect_files.R")
source("02_clean_each_source.R")
source("03_build_panel.R")
source("04_fit_bayesian_model.R")
source("05_diagnostics_and_outputs.R")
```

## Notes

- The outcome is `log(price_index)`.
- The policy-rate series is daily in `Interest_rate.csv`, so it is converted to monthly means to align with the monthly housing-price series.
- The housing-starts file now contains city-specific series for the six target geographies, so housing supply is merged directly at the city-month level.
- `brms`, `rstan`, `posterior`, and `bayesplot` are installed locally and the pipeline now uses them directly.
