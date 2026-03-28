# Housing Price Analysis in Major Ontario Cities

This project builds an annual city-year panel for six Ontario CMAs (2016–2023) and fits two competing Bayesian multilevel models to study how international student enrolment, the Bank of Canada policy rate, and housing supply interact to drive housing prices.

## Research question

How do interest rates, international student enrolment, and housing supply interact to drive housing prices across major Ontario cities?

## Data sources

| File | Contents | Aggregation |
|---|---|---|
| `Housing_price_index.csv` | Statistics Canada NHPI (18-10-0205-01) | Monthly → annual mean |
| `Interest_rate.csv` | Bank of Canada daily overnight-rate target | Daily → annual mean |
| `International_students.csv` | StatCan 37-10-0232-01 — postsecondary enrolment by institution | Annual sum per CMA |
| `New_houses_built.csv` | CMHC housing starts by CMA | Monthly → annual sum |

**Suppressed values (`..`) in the international students file** are imputed: years before an institution existed are set to 0; interior gaps are filled with the institution's mean across available years.

## Panel structure

- **Unit of analysis:** city × year
- **Cities:** Toronto, Ottawa-Gatineau, Hamilton, Kitchener-Cambridge-Waterloo, London, St. Catharines-Niagara
- **Years:** 2016–2023 (upper bound set by the international students data)
- **Observations:** 48 (6 cities × 8 years)
- **Outcome:** `log(HPI)` — log of the annual mean New Housing Price Index

## Models

**Model 1 (Base)**
```
log_hpi ~ scale(intl_students) + policy_rate + (1 + scale(intl_students) | cma)
```
Estimates the baseline effect of international student enrolment on log(HPI) with a varying intercept and varying student slope per CMA.

**Model 2 (Supply-Adjusted)**
```
log_hpi ~ scale(intl_students) + policy_rate + scale(house_supply) + (1 + scale(intl_students) + scale(house_supply) | cma)
```
Adds housing starts as a supply-side control with both a fixed effect and a city-varying slope, allowing the supply-price relationship to differ across CMAs. Comparing the `scale(intl_students)` coefficient between Model 1 and Model 2 is the key test for omitted variable bias.

**Priors**

| Parameter | Prior | Rationale |
|---|---|---|
| Intercept | `normal(4.8, 0.5)` | log(HPI) spans 4.57–5.15; weakly informative anchor |
| `scale(intl_students)` | `skew_normal(0.1, 0.1, 3)` | Slight positive prior (demand hypothesis); data can override |
| `policy_rate`, `scale(house_supply)` | `normal(0, 0.5)` | Weakly informative, no directional assumption |
| Group-level SD | `exponential(1)` | Controls shrinkage across 6 CMAs |
| Residual SD | `exponential(1)` | |
| Random effects correlation matrix | `lkj(2)` | Mild regularisation; 2×2 in Model 1 (intercept, student slope), 3×3 in Model 2 (intercept, student slope, supply slope) |

## Project structure

```
data/
  data_raw/       original source files
  clean/          cleaned tables and merged panel (panel_city_year.csv)
output/
  eda/            EDA report and city x year tables
  model/          fitted model RDS files and specification note
  loo/            LOO results, pointwise ELPD boxplot, comparison report
  ppc/            posterior predictive check plots and interpretation guide
  notes/          source and harmonization notes
  tables/         panel missingness table
```

## Scripts

| Script | Purpose |
|---|---|
| `01_inspect_files.R` | Inventory raw files and document their structure |
| `02_clean_each_source.R` | Clean each source and save to `data/clean/` |
| `03_build_panel.R` | Merge cleaned sources into `panel_city_year.csv` |
| `04_exploratory_data_analysis.R` | EDA report: distributions, trends, correlations, missingness |
| `05_fit_models.R` | Fit Model 1 and Model 2 in brms |
| `06_loo_comparison.R` | LOO-CV, pointwise diagnostics by CMA, model comparison |
| `07_posterior_predictive_checks.R` | PPC interval plots grouped by CMA |

## Run order

```r
source("01_inspect_files.R")
source("02_clean_each_source.R")
source("03_build_panel.R")
source("04_exploratory_data_analysis.R")
source("05_fit_models.R")
source("06_loo_comparison.R")
source("07_posterior_predictive_checks.R")
```

## Known limitations

- **Small panel (n = 48):** Only 8 time points per city limits the precision of city-level slope estimates.
- **LOO Pareto-k diagnostics:** After moment matching, 2 observations in Model 1 and 4 in Model 2 retain Pareto-k > 0.7, indicating those specific city-years are highly influential. With only 48 observations this is expected. The LOO comparison conclusion is not materially affected, but exact leave-one-out refitting (`reloo = TRUE`) was omitted due to the computational cost.
- **Annual aggregation:** International student data is only available annually, so the entire panel operates at annual frequency. Monthly variation in housing prices and the policy rate is averaged out.
- **CMA mapping:** International student counts are aggregated from institution-level data using a manual CMA lookup. Institutions not in the lookup are dropped.

## Dependencies

`brms`, `rstan`, `ggplot2` — install via `install.packages(c("brms", "rstan", "ggplot2"))`.
