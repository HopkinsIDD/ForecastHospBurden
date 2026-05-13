# Forecasting Hospital Burden from COVID-19

**What this does.** Take a daily admission forecast for COVID-19 hospitalizations and turn it into a forecast of the **census** — the number of patients in the hospital each day. Hospitals plan around census, but most public forecasting systems (COVIDhub, FluSight) only publish admissions. This pipeline bridges the two.

**Why it works.** If we know the probability that a patient is still hospitalised $d$ days after admission, today's census is past admissions weighted by that probability:

$$
\text{census}(t) = \sum_{s \le t} \text{admissions}(s) \cdot P(\text{LOS} > t - s)
$$

The modelling task reduces to estimating the length-of-stay (LOS) survival function $P(\text{LOS} > d)$ from historical data, then applying it to admission *forecasts*.

---

## Pipeline

```mermaid
flowchart TD
    A["<b>HHS data</b><br/>daily admissions + census per state"] --> B["<b>Fit LOS</b><br/>negbin per state × season<br/>(minimise SSE)"]
    C["<b>Hubverse admission forecast</b><br/>quantile paths per state"] --> D["<b>Convolve</b><br/>admissions ∗ P(LOS > d)<br/>using previous season's LOS"]
    B --> D
    D --> E["<b>Census forecast</b><br/>quantile paths per state"]
    E --> F["<b>Validate</b><br/>fan chart · WIS by horizon"]
```

---

## Data

| Field | Description |
|---|---|
| Source | [HHS COVID-19 Reported Patient Impact and Hospital Capacity](https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh) |
| `admissions` | Daily new confirmed COVID hospital admissions (adult + pediatric) |
| `census` | Total confirmed COVID inpatients (adult + pediatric) |
| Granularity | Daily, per US state |

---

## Model

We assume length of stay follows a **negative-binomial distribution** with mean $\mu$ and dispersion $k$. One $(\mu, k)$ pair is estimated per (state, respiratory season). The fitted distribution gives a survival function $P(\text{LOS} > d)$ which, when convolved with admissions, predicts census.

We estimate $\mu$ and $k$ by minimising the squared error between the observed census and the convolution of admissions with the implied survival function. Optimisation uses L-BFGS-B in log space so the parameters stay positive. Parameter uncertainty comes from a **residual bootstrap** — 100 refits on resampled residuals.

Three other LOS families (normal, lognormal, geometric) are implemented in `distributions.R` for comparison; the production pipeline uses negbin only.

**`MAX_STAY = 50` days** is the largest LOS we model, with $P(\text{LOS} > 50) \approx 0$. It governs three things:

- the length of every survival vector,
- the burn-in dropped at the start of each fit window (early census depends on unobserved prior admissions),
- the days of observed admissions prepended before each forecast, so day 1 already has a full history.

---

## Out-of-sample forecasting

For each (state, forecast_date) we convolve the COVIDhub admission quantile forecast with the **previous same-kind season's LOS** — Winter 2024-25 uses Winter 2023-24, Summer 2024 uses Summer 2023. No current-season data enters the LOS at forecast time, so evaluation is genuinely out-of-sample.

We run this two ways and keep both for diagnostics:

- **`ensemble+LOS`** — hub admission forecast → census. The production output. Spread is inherited from the hub.
- **`truth+LOS`** — observed admissions → census, with spread from the LOS bootstrap. This is the LOS-only error floor: census error if admissions were known perfectly.

The difference between the two decomposes census error into a part attributable to the upstream admission forecast and a part attributable to LOS estimation.

---

## Validation

- **Fan chart** per (state, forecast_date): 50% and 95% prediction intervals for admissions and census, observed overlaid.
- **WIS by horizon** via `scoringutils`, averaged per state.
- **Census error decomposition**: stack `truth+LOS` (LOS floor) and the residual `ensemble+LOS − truth+LOS` (admission-induced) per horizon.

---

## Project structure

```
source/
├── main.R                 # end-to-end pipeline
├── helpers/
│   ├── packages.R         # library imports
│   ├── data.R             # load_hhs(), load_hub()
│   ├── seasons.R          # season_of(), previous_season()
│   ├── distributions.R    # MAX_STAY + dist_* survival kernels
│   ├── los.R              # predict_census(), fit_los(), fit_los_all()
│   ├── forecast.R         # forecast_from_hub(), forecast_from_truth()
│   ├── baseline.R         # baseline_from_observed() — rWIS reference
│   ├── score.R            # score_forecast(), relative_wis()
│   └── plots.R            # plot_trajectories()
└── misc/                  # off-pipeline experiments
```
