# AI in International Market Entry: Tesla in China vs. BYD in Germany

Analysis code and data for my MSc thesis *Strategic Use of AI in International Market Entry: A Comparative Study*
(International Business Studies, FAU Erlangen-Nuremberg, Chair of Information Systems I, 2026).

## Question

Does stronger AI integration help a technology company succeed in a foreign market, or do localization,
user engagement and host-market conditions matter more?

## Data

No single dataset covered AI-enabled features, localization and user behaviour, so I built one.

- **Unit of observation:** one company in one host market in one quarter (Tesla in China, BYD in Germany, 2023 Q1 to 2025 Q4), 24 observations.
- **Sources:** company reports, product specifications, OTA update logs and release notes, app-store ratings and reviews, patent data, vehicle sales, brand rankings, GDP and passenger-car sales.
- **Variables:** 21 indicators coded or collected per quarter, combined into five composite indices: AI Integration, Localization Depth, User Engagement, Market Success and Host Market Capacity.
- **Qualitative layer:** app-store reviews coded by theme (usability, UX, reliability, trust and others) for both companies.

## Method

1. Normalize every indicator to a 0-1 scale (min-max for counts, fixed scales for ordinal codes and ratings), across the whole dataset to keep both cases comparable.
2. Aggregate indicators into composite indices with equal weights at dimension level (following OECD/JRC guidance on composite indicators).
3. Descriptive statistics and missing-data overview.
4. Pearson and Spearman correlations.
5. Three OLS models:
   - Model 1: Localization Depth ~ AI Integration
   - Model 2: User Engagement ~ AI Integration + Localization Depth
   - Model 3: Market Success ~ User Engagement + Host Market Capacity
6. Robustness check: Model 3 re-estimated without the outlier quarter (Tesla 2025 Q4).

## Key results

| Finding | Result |
|---|---|
| AI integration alone did not explain localization depth | Model 1: R² = 0.005 |
| Localization depth and market success | r = 0.78 |
| Host-market capacity and market success | r = 0.91 |
| Host-market capacity drives market success | Model 3: coefficient 0.48, p < 0.001, R² = 0.90 (adjusted 0.88) |
| Result holds without the outlier quarter | Robustness model: R² = 0.89 |

**Takeaway:** technology does not replace adaptation to the market. Localization, user trust, infrastructure and regulation shaped outcomes more than AI capability itself.

**Limitations:** small sample (24 quarters; user-engagement data available for 13), proxy indicators from public data, and constructed indices. Results are read as evidence of relationships, not as causal estimates.

## Repository structure

```
analysis.R                                   full analysis script
data/analysis_dataset.xlsx                   quarterly dataset used in the models
data/source_data_and_review_coding.xlsx      source tables, variable inputs and review coding
outputs/analysis_results.xlsx                normalized indicators, indices, statistics and model outputs
```

## How to run

Open the repository folder in RStudio (or set it as the working directory) and run `analysis.R`.
Packages: `readxl`, `dplyr`, `writexl`, `car`.
