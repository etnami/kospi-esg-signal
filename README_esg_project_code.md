# final_R_code.R

*Machine Learning and LLM-Assisted Analysis of Social ESG Indicators in KOSPI-Listed Korean Firms*

## Overview
This is a single, self-contained R script covering the full analysis
pipeline described in Chapter 3 (Methodology) and Chapter 4 (Results):
data cleaning and consolidation, exploratory data analysis, the four
predictive models answering RQ1 (Lasso, Elastic Net, Random Forest,
XGBoost), the OLS regression answering RQ2, the Stage 1 LLM
extraction-accuracy analysis (Claude Sonnet vs DeepSeek), and a final
sensitivity check on the Doosan Bobcat imputation. It is not split
into separate files; all steps run in sequence within this one
script.



## Requirements
R version 4.5.1 or later.

Required packages (35 unique, confirmed directly from this script's
library() calls):

```
readr, readxl, dplyr, tidyr, stringr, janitor, openxlsx, mice, VIM,
glmnet, ranger, xgboost, DALEX, caret, rsample, yardstick, irr,
psych, lmtest, sandwich, broom, ggplot2, ggcorrplot, patchwork,
ggrepel, scales, RColorBrewer, knitr, kableExtra, stargazer, gt,
moments, gridExtra, grid
```

Note: grid ships with base R and does not need separate installation.
lmtest and sandwich are each loaded twice in the script (once near
the top, once again just before the RQ2 diagnostics); this is
harmless and doesn't affect the package list below.

Install the rest with:

```r
install.packages(c(
  "readr", "readxl", "dplyr", "tidyr", "stringr", "janitor",
  "openxlsx", "mice", "VIM", "glmnet", "ranger", "xgboost", "DALEX",
  "caret", "rsample", "yardstick", "irr", "psych", "lmtest",
  "sandwich", "broom", "ggplot2", "ggcorrplot", "patchwork",
  "ggrepel", "scales", "RColorBrewer", "knitr", "kableExtra",
  "stargazer", "gt", "moments", "gridExtra"
))
```

Package roles, grouped by purpose:
  - Data import/cleaning: readr, readxl, dplyr, tidyr, stringr,
    janitor, openxlsx
  - Missing data: mice, VIM
  - Modelling (RQ1): glmnet (Lasso, Elastic Net), ranger (Random
    Forest), xgboost, DALEX (model explainability/importance),
    caret, rsample, yardstick (resampling/evaluation workflow)
  - Reliability/agreement: irr (ICC, inter-model agreement), psych
  - Regression diagnostics (RQ2): lmtest, sandwich, broom
  - Visualisation: ggplot2, ggcorrplot, patchwork, ggrepel, scales,
    RColorBrewer, gridExtra, grid
  - Table/output formatting: knitr, kableExtra, stargazer, gt
  - Descriptive statistics: moments (skewness, kurtosis)



## Data
This script expects the following input files to be present in the
working directory (the script reads them by bare filename, so it
assumes they sit alongside the script; see Script structure, Section
3, for the exact read.csv()/read_excel() calls):

  `final dataset.csv` - the consolidated 74-firm dataset containing
  Set A (24 indicators, summed 2018-2024), Set B (23 indicators, 2024
  only), and OverallRank. Note the lowercase "final" and the space
  before "dataset": the filename in the script does not match the
  capitalised "Final dataset.csv" implied elsewhere; rename your file
  to match exactly, or edit the read.csv() call near the top of
  Section 3.

  `share_price.csv` - share price data (indexed to 2018 = 100) for the
  RQ2 subsample of the original 55 companies (Methodology Sections
  3.2.3, 3.3.4). The script pivots this from long to wide format
  (one Share_Price_<year> column per year) before merging it into the
  main dataset.

  `extraction_log.xlsx` - required only for the Stage 1 LLM extraction
  accuracy analysis, near the end of the script. The script reads the
  "Flags to Review" sheet specifically (read_excel(..., sheet =
  "Flags to Review")) and expects 11 columns in a fixed order, which
  it renames to: Company, Code, Year, Indicator, Claude, DeepSeek,
  Diff_Pct, Diagnostic_Note, Corrected, Source, Resolved.

Set your working directory to the folder containing these files
before running the script (all three read calls and all output
files use bare filenames, i.e. relative paths).


## Script structure (in run order)

The script is organised into the following named sections, in the
order they appear:

```
 1. LOADING LIBRARIES
 2. DEFINING COLOUR PALETTE: Okabe-Ito, colour-blind-friendly,
    used for all plots
 3. LOADING DATA: reads final dataset.csv and share_price.csv,
    pivots share price to wide format
 4. MERGING DATASETS: left-joins share price onto the main dataset
 5. CLEANING INDUSTRY LABELS
 6. SELECTING PREDICTORS: Indicators 1-24 only (25-30 are external
    ratings, excluded)
 7. CHECKING MISSINGNESS
 8. IMPUTATION: mean imputation for Doosan Bobcat's 2 missing
    values (MNAR judged likely; mean imputation chosen as
    transparent and defensible); done before standardising
 9. STANDARDISATION: z-score standardisation, after imputation
10. EDA: SUMMARY STATISTICS (Table 3.7)
11. EDA: OUTCOME SUMMARY TABLE (APA style)
12. EDA: STRIP PLOT: ESG rank by industry, plus a Kruskal-Wallis
    test of whether sectors differ significantly in ESG rank
    (non-parametric alternative to ANOVA, chosen for unequal group
    sizes)
13. EDA: CORRELATION MATRIX, plus Spearman correlations of each
    indicator against Overall Rank (a direct preview of RQ1)
14. EDA: INDICATOR DISTRIBUTIONS (split into two plots)
15. EDA: OUTLIER HEATMAP (only companies with at least one |z| > 2
    shown)
16. EDA: SCATTERPLOTS (split into two plots)
17. EDA: SET A VS SET B COMPARISON (split into two plots)
18. EDA: TOP AND BOTTOM PERFORMERS
19. EDA: DISCLOSURE RATES OVER TIME
20. EDA: RQ2 SHARE PRICE DISTRIBUTION
21. EDA: RQ2 ESG RANK VS SHARE PRICE
22. EDA: SECTOR BREAKDOWN OF KEY INDICATORS (sectors with 4+
    companies only)

RQ1: PREDICTIVE MODELLING
    MODEL 1: LASSO REGRESSION: primary model; alpha = 1; LOOCV for
    lambda selection (nfolds = nrow); run on both Set A and Set B
    MODEL 2: ELASTIC NET REGRESSION: primary model; alpha = 0.5;
    LOOCV for lambda selection; run on both Set A and Set B
    MODEL 3: RANDOM FOREST (WITH HYPERPARAMETER TUNING): robustness
    check; permutation importance (not impurity, following Strobl et
    al. 2007, to avoid inflating continuous variables); mtry and
    min.node.size tuned via OOB error; run on both Set A and Set B
    MODEL 4: XGBOOST (WITH HYPERPARAMETER TUNING): robustness check;
    gain importance; max_depth/eta/subsample tuned via 10-fold CV
    (not full LOOCV, to keep runtime reasonable); nrounds picked via
    early stopping within xgb.cv; a manual LOOCV loop then provides
    the final honest performance evaluation with the tuned
    hyperparameters fixed; run on both Set A and Set B
    CROSS-VALIDATION: PERFORMANCE ESTIMATES FOR ALL FOUR MODELS:
    Lasso/Elastic Net use LOOCV via cv.glmnet, Random Forest uses
    OOB, XGBoost's LOOCV is added here; LOOCV RMSE extracted for
    Lasso/Elastic Net for comparison
    SET B CHECKS: missingness (full detail, pre-imputation),
    descriptive statistics (skew, kurtosis), multicollinearity,
    outliers, and an empirical check of whether missingness relates
    to prediction error
    CROSS-MODEL IMPORTANCE COMPARISON: summarising which indicators
    are robust across all four models (Table 4.4)
    FEATURE IMPORTANCE COMPARISON: SET A vs SET B: side by side for
    all four models

RQ2: OLS REGRESSION
    Regresses composite Social ESG ranking on December 2022 share
    price for the 55 original companies (share price indexed to
    2018 = 100)
    RQ2 OLS ASSUMPTION TESTS: heteroskedasticity (Breusch-Pagan) and
    normality of residuals (Shapiro-Wilk), plus Cook's distance and
    HC3 robust standard errors

RESULTS TABLES: APA-style summary tables for the dissertation,
    saved as .html via gt::gtsave()

RESULTS VISUALISATIONS: predicted-vs-actual and residual plots for
    all four models, plus an RMSE comparison chart

STAGE 1: LLM EXTRACTION ACCURACY ANALYSIS: compares Claude Sonnet
    vs DeepSeek extraction outputs against ground truth, using exact
    match rate, MAE (with and without Annual Pay, whose KRW scale
    inflates raw differences), close-match rate (within 5%), and ICC.
    Reads extraction_log.xlsx (see Data, above). Also re-saves a
    block of EDA figures that were generated earlier in the script
    but not yet written to disk at that point.

SENSITIVITY CHECK: Doosan Bobcat mean imputation: rebuilds the
    dataset with Doosan Bobcat's row excluded entirely (N=73) rather
    than mean-imputed (N=74), and re-runs Lasso and Random Forest on
    both versions to compare LOOCV RMSE, OOB RMSE, and the top
    indicator lists.

Summary: prints the headline sensitivity-check figures (percentage
    change in Lasso LOOCV RMSE and Random Forest OOB RMSE between the
    N=74 imputed and N=73 excluded versions) for use in the write-up.
```



## Outputs produced
Running the script in full writes the following files to the working
directory (all as bare, relative filenames; no output subfolder is
created):

Figures (.png), in the order they're produced:

```
table1_outcome_summary.png, spearman_correlations.png,
04b_correlation_matrix_setB.png, model_performance_comparison.png,
feature_importance_comparison.png, rq2_scatter.png,
rq2_quartile_trend.png, pred_vs_actual_setA.png,
residuals_setA.png, rq2_pred_vs_actual.png, rq2_residuals.png,
rmse_comparison.png, stage1_accuracy.png, stage1_error_types.png,
stage1_error_heatmap.png, stage1_accuracy_by_company.png,
stage1_error_types_refined.png, stage1_error_types_final.png,
table5_stage1_summary.png, 03_rank_by_industry.png,
04_correlation_matrix.png, 05_correlated_pairs.png,
06a_indicator_distributions_01to12.png,
06b_indicator_distributions_13to24.png, 07_outlier_heatmap.png,
08a_indicator_vs_rank_01to12.png, 08b_indicator_vs_rank_13to24.png,
09a_setA_vs_setB_part1.png, 09b_setA_vs_setB_part2.png,
10_top_bottom_performers.png, 11_disclosure_rates_over_time.png
```

Note: table1_outcome_summary.png is written twice (once around
  Section 11 and again in the later "saving all missing EDA plots"
  block); the second call simply overwrites the first with the same
  content.

Tables (.html, via gt::gtsave()):

```
table2_predictor_descriptives.html, table2_model_performance.html,
table3_robust_indicators.html, table4_rq2_regression.html,
table2_model_performance_updated.html
```

The Elastic Net section (Model 2) prints its plot to the R graphics
device only (print(p_enet)) and does not save it to a file; the
script's own comment notes "Save plot manually if satisfied."



## Notes
- This script was developed and run in R 4.5.1. Package versions
  were not pinned; if a package's API has changed since this script
  was written, minor adjustments may be needed.
- Random seed: set.seed(123) is used throughout, before each of the
  Lasso and Elastic Net LOOCV fits, before every Random Forest and
  XGBoost hyperparameter-tuning grid combination and again before
  each final refit, before the results-visualisation model refits,
  and before both models in the Doosan Bobcat sensitivity check. The
  same seed (123) is reused every time rather than varied across
  runs.
- Runtime: not stated in the script (no timing code is included).
  The Random Forest and XGBoost hyperparameter-tuning grids and the
  manual LOOCV loops are the most likely sources of longer runtime;
  time a full run on your machine and note it here if needed.
- The Stage 1 LLM extraction analysis and the Doosan Bobcat
  sensitivity check are both present in the script but were not
  mentioned in the original outline of this README; they are
  included above.

