# ════════════════════════════════════════════════════════════════
# SECTION 1: LOADING LIBRARIES
# ════════════════════════════════════════════════════════════════

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(openxlsx)
library(mice)
library(VIM)
library(glmnet)
library(ranger)
library(xgboost)
library(DALEX)
library(caret)
library(rsample)
library(yardstick)
library(irr)
library(psych)
library(lmtest)
library(sandwich)
library(broom)
library(ggplot2)
library(ggcorrplot)
library(patchwork)
library(ggrepel)
library(scales)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(stargazer)
library(gt)
library(moments)
library(gridExtra)
library(grid)
library(lmtest)
library(sandwich)

# ════════════════════════════════════════════════════════════════
# SECTION 2: DEFINING COLOUR PALETTE
# Okabe-Ito — colour blind friendly — used for all plots
# ════════════════════════════════════════════════════════════════

okabe_ito <- c(
  orange     = "#E69F00",
  sky_blue   = "#56B4E9",
  green      = "#009E73",
  yellow     = "#F0E442",
  blue       = "#0072B2",
  vermillion = "#D55E00",
  pink       = "#CC79A7",
  black      = "#000000"
)
plot_blue      <- unname(okabe_ito["blue"])
plot_highlight <- unname(okabe_ito["vermillion"])
plot_two       <- unname(c(okabe_ito["blue"], okabe_ito["orange"]))
plot_full      <- unname(okabe_ito)

# ════════════════════════════════════════════════════════════════
# SECTION 3: LOADING DATA
# ════════════════════════════════════════════════════════════════

#Loading main dataset
final_dataset <- read.csv("final dataset.csv",
                          stringsAsFactors = FALSE,
                          fileEncoding     = "UTF-8-BOM")

cat("Main dataset loaded:", nrow(final_dataset), "rows x",
    ncol(final_dataset), "cols\n")

#Loading share price data
share_price <- read.csv("share_price.csv",
                        stringsAsFactors = FALSE)

cat("Share price loaded:", nrow(share_price), "rows\n")

#Pivoting share price to wide format
share_price_wide <- share_price %>%
  pivot_wider(
    id_cols      = Company,
    names_from   = Year,
    names_prefix = "Share_Price_",
    values_from  = Share_Price_Close_Indexed
  )

cat("Share price pivoted:", nrow(share_price_wide), "companies x",
    ncol(share_price_wide), "cols\n")

# ════════════════════════════════════════════════════════════════
# SECTION 4: MERGING DATASETS
# ════════════════════════════════════════════════════════════════

complete_data <- final_dataset %>%
  left_join(share_price_wide, by = "Company")

cat("\nMerged dataset:", nrow(complete_data), "rows x",
    ncol(complete_data), "cols\n")
cat("Share price coverage (2018):",
    sum(!is.na(complete_data$Share_Price_2018)), "companies\n")

# ════════════════════════════════════════════════════════════════
# SECTION 5: CLEANING INDUSTRY LABELS
# ════════════════════════════════════════════════════════════════

complete_data <- complete_data %>%
  mutate(Industry_Clean = gsub("\\s*\\d+$", "", Industry_KRX),
         Industry_Clean = gsub("\\\\n$",    "", Industry_Clean),
         Industry_Clean = trimws(Industry_Clean))

cat("\nIndustries after cleaning:\n")
print(sort(table(complete_data$Industry_Clean), decreasing = TRUE))

# ════════════════════════════════════════════════════════════════
# SECTION 6: SELECTING PREDICTORS
# Indicators 1-24 only (25-30 are external ratings)
# ════════════════════════════════════════════════════════════════

#Identifying the 24 employment indicator prefixes (X01 through X24)
indicator_prefixes <- unique(
  gsub("_(2018|2019|2020|2021|2022|2023|2024|Total)$", "",
       names(complete_data)[
         grepl("^X0[1-9]_|^X1[0-9]_|^X2[0-4]_", names(complete_data))
       ])
)

cat("\nEmployment indicator prefixes found:", length(indicator_prefixes), "\n")
stopifnot(length(indicator_prefixes) == 24)

#Recomputing each indicator's "_Total" as a genuine row-wise sum
#across whichever years that company has data for, matching Matanle
#et al.'s own documented convention. Firms missing ALL years for a
#given indicator (i.e. Doosan Bobcat on two indicators) are explicitly
#set back to NA rather than a false zero, since rowSums(na.rm=TRUE)
#on an all-NA row would otherwise silently return 0.
for (prefix in indicator_prefixes) {
  year_cols <- paste0(prefix, "_", 2018:2024)
  year_cols <- year_cols[year_cols %in% names(complete_data)]
  total_col <- paste0(prefix, "_Total")
  
  year_matrix <- as.matrix(complete_data[, year_cols, drop = FALSE])
  all_na      <- rowSums(!is.na(year_matrix)) == 0
  
  row_totals         <- rowSums(year_matrix, na.rm = TRUE)
  row_totals[all_na]  <- NA
  
  complete_data[[total_col]] <- row_totals
}

cat("Set A '_Total' columns corrected: now genuine row-wise sums\n")
cat("(matching Matanle et al.'s documented convention; firms with\n")
cat("zero years of data for an indicator correctly left as NA).\n\n")

#Total columns (Set A — historical sum 2018-2024)
predictor_cols <- names(complete_data)[
  grepl("_Total$", names(complete_data)) &
    grepl("^X0[1-9]_|^X1[0-9]_|^X2[0-4]_", names(complete_data))
]

#2024 columns (Set B — most recent year) — not affected by this,
#since these are single-year raw values, never summed across years
cols_2024 <- names(complete_data)[
  grepl("_2024$", names(complete_data)) &
    grepl("^X0[1-9]_|^X1[0-9]_|^X2[0-4]_", names(complete_data))
]

#Removing X07 from Set B (50% missing in 2024)
pred_B <- cols_2024[!grepl("X07_", cols_2024)]

cat("\nSet A predictors:", length(predictor_cols), "\n")
cat("Set B predictors:", length(pred_B), "(X07 dropped)\n")

# ════════════════════════════════════════════════════════════════
# SECTION 7: CHECKING MISSINGNESS
# ════════════════════════════════════════════════════════════════

miss_summary <- complete_data %>%
  select(all_of(predictor_cols)) %>%
  summarise(across(everything(),
                   ~round(mean(is.na(.)) * 100, 1))) %>%
  pivot_longer(everything(),
               names_to  = "Indicator",
               values_to = "Pct_Missing") %>%
  arrange(desc(Pct_Missing))

cat("\nMissingness in Total columns:\n")
print(miss_summary %>% filter(Pct_Missing > 0))
cat("Companies with missing values:\n")
print(complete_data %>%
        select(Company, all_of(predictor_cols)) %>%
        mutate(n_missing = rowSums(is.na(
          select(., all_of(predictor_cols))))) %>%
        filter(n_missing > 0) %>%
        select(Company, n_missing))

# ════════════════════════════════════════════════════════════════
# SECTION 8: IMPUTATION
# Mean imputation - only 2 missing values (Doosan Bobcat)
# MNAR likely - mean imputation transparent and defensible
# Imputing BEFORE standardising
# ════════════════════════════════════════════════════════════════

#Set A imputation
imputed_A <- complete_data %>%
  select(all_of(predictor_cols)) %>%
  mutate(across(everything(),
                ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

#Set B imputation
imputed_B <- complete_data %>%
  select(all_of(pred_B)) %>%
  mutate(across(everything(),
                ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

cat("\nImputation complete.\n")
cat("Missing in Set A after imputation:", sum(is.na(imputed_A)), "\n")
cat("Missing in Set B after imputation:", sum(is.na(imputed_B)), "\n")
cat("Doosan Bobcat NonKorean (imputed):",
    round(imputed_A$X05_NonKoreanCount_Total[
      complete_data$Company == "Doosan Bobcat"], 1), "\n")
cat("Doosan Bobcat UnionMembership (imputed):",
    round(imputed_A$X22_UnionMembershipPct_Total[
      complete_data$Company == "Doosan Bobcat"], 1), "\n")

# ════════════════════════════════════════════════════════════════
# SECTION 9: STANDARDISATION
# Z-score standardisation AFTER imputation
# ════════════════════════════════════════════════════════════════

X_A <- scale(imputed_A)
X_B <- scale(imputed_B)
y   <- complete_data$OverallRank

cat("\nStandardisation complete.\n")
cat("X_A:", nrow(X_A), "x", ncol(X_A), "\n")
cat("X_B:", nrow(X_B), "x", ncol(X_B), "\n")
cat("y: range", min(y), "to", max(y), "| NAs:", sum(is.na(y)), "\n")

#Creating short indicator names and raw unstandardised predictor dataframe
short_names <- gsub("_Total$", "", predictor_cols)
short_names <- gsub("^X\\d+_", "", short_names)
pred_raw    <- imputed_A
colnames(pred_raw) <- short_names

cat("\nAll preprocessing complete. Ready for EDA.\n")

# ════════════════════════════════════════════════════════════════
# SECTION 10: EDA — SUMMARY STATISTICS
# ════════════════════════════════════════════════════════════════

cat("\n=== OUTCOME VARIABLE SUMMARY (OverallRank) ===\n")
outcome_summary <- data.frame(
  Statistic = c("Minimum", "Maximum", "Mean", "Median",
                "Standard Deviation", "Skewness", "Kurtosis",
                "N", "Missing"),
  Value = c(
    min(y), max(y),
    round(mean(y), 2), median(y),
    round(sd(y), 2),
    round(skewness(y), 3),
    round(kurtosis(y), 3),
    length(y),
    sum(is.na(y))
  )
)
print(outcome_summary)

cat("\n=== PREDICTOR SUMMARY (Set A, raw unstandardised) ===\n")
cat("Using psych::describe for full distributional summary\n\n")
pred_desc <- describe(pred_raw) %>%
  select(n, mean, sd, median, min, max, skew, kurtosis) %>%
  round(2)
print(pred_desc)


pred_desc_table <- data.frame(
  Indicator = c("Contract Workers %", "Female Permanent Workers %",
                "Female Contract Workers %", "Disabilities Count",
                "Non-Korean Count", "New Hires %",
                "Contract New Hires %", "New Female Workers %",
                "Female Middle Managers %", "Female Executives %",
                "Female Board Directors %", "Annual Pay",
                "Gender Pay Gap", "Training Budget",
                "Training Hours", "Parental Leave (Female)",
                "Parental Leave (Male)", "Return to Work %",
                "Length of Service", "Voluntary Turnover %",
                "Involuntary Turnover %", "Union Membership %",
                "LTIFR", "Fatality Rate"),
  N        = rep(74, 24),
  Mean     = c(227.74,213.95,187.86,218.70,181.92,215.96,139.15,174.73,
               211.38,206.70,139.95,186.78,184.54,221.84,224.01,216.07,
               213.49,189.19,173.47,217.95,192.66,177.07,211.41,215.89),
  SD       = c(110.84,111.26,97.43,124.51,78.27,113.30,86.92,77.79,
               100.01,99.59,47.74,80.68,114.01,105.72,116.70,92.59,
               105.67,93.17,87.21,105.14,98.97,89.57,95.63,91.60),
  Median   = c(225.50,210.00,196.50,226.00,181.46,226.50,143.50,170.00,
               203.50,210.00,142.00,194.50,193.00,223.00,210.50,231.00,
               222.00,203.00,168.50,218.00,186.50,178.00,201.50,208.50),
  Min      = c(4,2,25,2,13,2,4,46,7,10,34,17,7,8,24,30,4,19,21,23,7,13,55,35),
  Max      = c(456,443,422,443,360,420,291,381,404,387,240,357,411,451,
               467,423,399,363,370,460,437,365,396,398),
  Skewness = c(0.07,-0.03,0.12,-0.06,-0.07,-0.16,0.06,0.29,0.07,0.04,
               -0.08,-0.01,0.08,0.04,0.26,-0.02,-0.27,-0.19,0.23,0.28,
               0.06,-0.05,0.15,0.10),
  Kurtosis = c(-0.73,-0.94,-0.92,-1.11,-0.21,-0.92,-1.22,-0.85,-1.12,
               -0.91,-0.40,-0.78,-1.11,-0.91,-0.93,-0.85,-1.06,-0.97,
               -0.94,-0.81,-0.70,-0.76,-1.21,-0.81)
)

table_pred <- pred_desc_table %>%
  gt() %>%
  tab_header(
    title    = "Table 2",
    subtitle = "Descriptive Statistics for Employment Indicator Predictors (Set A — Historical Average 2018-2024)"
  ) %>%
  cols_label(
    Indicator = "Indicator",
    N         = "N",
    Mean      = "M",
    SD        = "SD",
    Median    = "Mdn",
    Min       = "Min",
    Max       = "Max",
    Skewness  = "Skewness",
    Kurtosis  = "Kurtosis"
  ) %>%
  tab_source_note(
    source_note = "Note. All values are indicator ranks on a 1-74 scale (1 = best performing company on that indicator, 74 = worst). N = 74 KOSPI-100 listed Korean firms. Values represent historical averages across 2018-2024. Two missing values (Doosan Bobcat: Non-Korean Count and Union Membership %) were imputed using mean imputation prior to analysis. Skewness and kurtosis values are within acceptable bounds (|skewness| < 2, kurtosis > -2), consistent with the approximately uniform distributions expected for rank-based data."
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides  = c("top", "bottom"),
      color  = "black",
      weight = px(2)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides  = "bottom",
      color  = "black",
      weight = px(2)
    ),
    locations = cells_body(rows = nrow(pred_desc_table))
  ) %>%
  tab_options(
    table.border.top.style             = "hidden",
    table.border.bottom.style          = "hidden",
    heading.border.bottom.color        = "black",
    heading.border.bottom.width        = px(1),
    column_labels.border.top.style     = "solid",
    column_labels.border.top.color     = "black",
    column_labels.border.top.width     = px(2),
    column_labels.border.bottom.style  = "solid",
    column_labels.border.bottom.color  = "black",
    column_labels.border.bottom.width  = px(1),
    table.font.names                   = "Times New Roman",
    table.font.size                    = px(11),
    data_row.padding                   = px(3),
    heading.title.font.size            = px(12),
    heading.subtitle.font.size         = px(11),
    source_notes.font.size             = px(9)
  )

gtsave(table_pred, "table2_predictor_descriptives.html")
cat("Saved: table2_predictor_descriptives.html\n")

# ════════════════════════════════════════════════════════════════
# SECTION 11: EDA — OUTCOME SUMMARY TABLE (APA style)
# ════════════════════════════════════════════════════════════════

table_grob <- tableGrob(
  outcome_summary,
  rows  = NULL,
  theme = ttheme_minimal(
    core    = list(fg_params = list(fontfamily = "serif",
                                    fontsize   = 11)),
    colhead = list(fg_params = list(fontfamily = "serif",
                                    fontsize   = 11,
                                    fontface   = "bold")),
    padding = unit(c(4, 8), "mm")
  )
)

p_table <- arrangeGrob(
  table_grob,
  top    = textGrob(
    "Table 1. Descriptive Statistics for Composite Social ESG Ranking",
    gp = gpar(fontfamily = "serif", fontsize = 12, fontface = "bold")),
  bottom = textGrob(
    "Note. Rankings range from 1 (best) to 74 (worst). Distribution is uniform by construction. N = 74.",
    gp = gpar(fontfamily = "serif", fontsize = 9, fontface = "italic"))
)

#Saving directly to PNG — bypasses viewport error
png("table1_outcome_summary.png", width = 800, height = 600, res = 150)
grid.draw(p_table)
dev.off()
cat("Saved: table1_outcome_summary.png\n")

# ════════════════════════════════════════════════════════════════
# SECTION 12: EDA — STRIP PLOT: ESG RANK BY INDUSTRY
# ════════════════════════════════════════════════════════════════

industry_data <- data.frame(
  Company     = complete_data$Company,
  OverallRank = y,
  Industry    = complete_data$Industry_Clean
)

industry_medians <- industry_data %>%
  group_by(Industry) %>%
  summarise(Median_Rank = median(OverallRank),
            N           = n(),
            .groups     = "drop")

p_industry <- ggplot(industry_data,
                     aes(x = reorder(Industry, OverallRank, median),
                         y = OverallRank)) +
  geom_jitter(aes(colour = Industry),
              width       = 0.25,
              size        = 3,
              alpha       = 0.85,
              show.legend = FALSE) +
  geom_crossbar(data      = industry_medians,
                aes(x     = reorder(Industry, Median_Rank),
                    y     = Median_Rank,
                    ymin  = Median_Rank,
                    ymax  = Median_Rank),
                width     = 0.5,
                colour    = unname(okabe_ito["black"]),
                linewidth = 0.8) +
  geom_text(data    = industry_medians,
            aes(x   = reorder(Industry, Median_Rank),
                y   = 78,
                label = paste0("n=", N)),
            size   = 3.5,
            colour = "grey40") +
  scale_colour_manual(
    values = rep(plot_full,
                 length.out = n_distinct(industry_data$Industry))) +
  scale_y_continuous(limits = c(1, 74), breaks = seq(0, 75, by = 15)) +
  coord_flip() +
  labs(
    title    = "Social ESG Ranking by Industry Sector",
    subtitle = "Each point = one company | Horizontal line = group median | Lower rank = better",
    x        = "",
    y        = "Overall Social ESG Rank",
    caption  = "Okabe-Ito colour blind friendly palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 10),
    axis.text.y        = element_text(size = 11),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92")
  )

print(p_industry)
cat("Plot 03 displayed. Save manually if satisfied.\n")

# ════════════════════════════════════════════════════════════════
# KRUSKAL-WALLIS: DO SECTORS DIFFER SIGNIFICANTLY IN ESG RANK?
# Non-parametric alternative to ANOVA (unequal group sizes)
# ════════════════════════════════════════════════════════════════

kw_test <- kruskal.test(OverallRank ~ Industry,
                        data = industry_data)
cat("=== KRUSKAL-WALLIS TEST: RANK BY INDUSTRY ===\n")
print(kw_test)
cat("Interpretation: if p < 0.05, sectors differ significantly in ESG rank\n")

# ════════════════════════════════════════════════════════════════
# SECTION 13: EDA — CORRELATION MATRIX
# ════════════════════════════════════════════════════════════════

# ════════════════════════════════════════════════════════════════
# SPEARMAN CORRELATIONS: EACH INDICATOR VS OVERALL RANK
# Directly previews RQ1 — which indicators correlate with rank
# ════════════════════════════════════════════════════════════════

spearman_results <- data.frame(
  Indicator = short_names,
  Rho       = sapply(short_names, function(ind) {
    cor(pred_raw[[ind]], y, method = "spearman")
  }),
  P_value   = sapply(short_names, function(ind) {
    cor.test(pred_raw[[ind]], y, method = "spearman")$p.value
  })
) %>%
  mutate(
    Significant = P_value < 0.05,
    Direction   = ifelse(Rho > 0, "Positive", "Negative")
  ) %>%
  arrange(desc(abs(Rho)))

cat("=== SPEARMAN CORRELATIONS: INDICATORS VS OVERALL RANK ===\n")
print(spearman_results, digits = 3)

p_spearman <- ggplot(spearman_results,
                     aes(x    = reorder(Indicator, abs(Rho)),
                         y    = Rho,
                         fill = Direction)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 0,
             colour     = unname(okabe_ito["black"]),
             linewidth  = 0.5) +
  geom_point(data = spearman_results %>% filter(Significant),
             aes(x = reorder(Indicator, abs(Rho)), y = Rho),
             shape  = 8,
             size   = 2,
             colour = unname(okabe_ito["black"])) +
  scale_fill_manual(values = c(
    "Positive" = plot_blue,
    "Negative" = plot_highlight
  )) +
  coord_flip() +
  labs(
    title    = "Spearman Correlations: Employment Indicators vs Social ESG Ranking",
    subtitle = "Asterisk (*) = statistically significant (p < 0.05) | Positive = higher indicator rank → worse ESG rank",
    x        = "", y = "Spearman Rho",
    fill     = "Direction",
    caption  = "Set A: Historical averages 2018-2024 | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

png("spearman_correlations.png", width = 1200, height = 900, res = 150)
print(p_spearman)
dev.off()
cat("Saved: spearman_correlations.png\n")


cor_matrix <- cor(imputed_A, use = "complete.obs")
rownames(cor_matrix) <- short_names
colnames(cor_matrix) <- short_names

p_corr <- ggcorrplot(
  cor_matrix,
  method   = "square",
  type     = "lower",
  lab      = TRUE,
  lab_size = 2.5,
  colors   = c(plot_highlight, "white", plot_blue),
  title    = "Correlation Matrix: Employment Indicators (Set A — Historical Average 2018-2024)",
  ggtheme  = theme_minimal(base_size = 9)
) +
  theme(
    plot.title  = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8)
  )

print(p_corr)
cat("Plot 04 displayed. Save manually if satisfied.\n")

#Checking for highly correlated pairs
cat("\nHighly correlated pairs (|r| > 0.7):\n")
cor_upper <- cor_matrix
cor_upper[lower.tri(cor_upper, diag = TRUE)] <- NA
high_cor  <- which(abs(cor_upper) > 0.7, arr.ind = TRUE)

if(nrow(high_cor) == 0) {
  cat("  No pairs above 0.7 found.\n")
} else {
  for(i in seq_len(nrow(high_cor))) {
    r1 <- rownames(cor_matrix)[high_cor[i,1]]
    r2 <- colnames(cor_matrix)[high_cor[i,2]]
    cat(" ", r1, "vs", r2, ":",
        round(cor_upper[high_cor[i,1], high_cor[i,2]], 3), "\n")
  }
  
  #Plotting correlated pairs
  pair_plots <- list()
  for(i in seq_len(min(nrow(high_cor), 6))) {
    r1 <- rownames(cor_matrix)[high_cor[i,1]]
    r2 <- colnames(cor_matrix)[high_cor[i,2]]
    r  <- round(cor_upper[high_cor[i,1], high_cor[i,2]], 3)
    df <- data.frame(
      x       = pred_raw[[r1]],
      y_val   = pred_raw[[r2]],
      Company = complete_data$Company
    )
    pair_plots[[i]] <- ggplot(df, aes(x = x, y = y_val)) +
      geom_point(colour = plot_blue, alpha = 0.6, size = 2.5) +
      geom_smooth(method    = "lm",
                  colour    = plot_highlight,
                  se        = FALSE,
                  linewidth = 0.8) +
      geom_text_repel(
        data = df %>% arrange(desc(abs(x - mean(x)))) %>% head(5),
        aes(label = Company),
        size         = 3,
        colour       = unname(okabe_ito["black"]),
        max.overlaps = 10
      ) +
      labs(x = r1, y = r2, title = paste0("r = ", r)) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())
  }
  p_pairs <- wrap_plots(pair_plots) +
    plot_annotation(
      title    = "Highly Correlated Indicator Pairs (|r| > 0.7)",
      subtitle = "Top 5 extreme companies labelled | Vermillion line = linear fit",
      caption  = "Okabe-Ito palette"
    )
  print(p_pairs)
  cat("Plot 05 displayed. Save manually if satisfied.\n")
}

# ════════════════════════════════════════════════════════════════
# SECTION 14: EDA — INDICATOR DISTRIBUTIONS (split into two plots)
# ════════════════════════════════════════════════════════════════

box_data <- pred_raw %>%
  pivot_longer(everything(),
               names_to  = "Indicator",
               values_to = "Value")

#Indicators 01-12
p_box_a <- pred_raw %>%
  select(1:12) %>%
  pivot_longer(everything(),
               names_to  = "Indicator",
               values_to = "Value") %>%
  ggplot(aes(x    = reorder(Indicator, Value, median),
             y    = Value,
             fill = Indicator)) +
  geom_boxplot(outlier.shape  = 21,
               outlier.fill   = plot_highlight,
               outlier.colour = "grey30",
               outlier.size   = 2,
               alpha          = 0.85,
               show.legend    = FALSE) +
  scale_fill_manual(values = rep(plot_full, length.out = 12)) +
  coord_flip() +
  labs(
    title    = "Distribution of Employment Indicators (01-12)",
    subtitle = "Set A: Historical averages 2018-2024 | Raw unstandardised values",
    x        = "", y = "Value",
    caption  = "Outliers in vermillion | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    axis.text.y      = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

print(p_box_a)
cat("Plot 06a displayed. Save manually if satisfied.\n")

#Indicators 13-24
p_box_b <- pred_raw %>%
  select(13:24) %>%
  pivot_longer(everything(),
               names_to  = "Indicator",
               values_to = "Value") %>%
  ggplot(aes(x    = reorder(Indicator, Value, median),
             y    = Value,
             fill = Indicator)) +
  geom_boxplot(outlier.shape  = 21,
               outlier.fill   = plot_highlight,
               outlier.colour = "grey30",
               outlier.size   = 2,
               alpha          = 0.85,
               show.legend    = FALSE) +
  scale_fill_manual(values = rep(plot_full, length.out = 12)) +
  coord_flip() +
  labs(
    title    = "Distribution of Employment Indicators (13-24)",
    subtitle = "Set A: Historical averages 2018-2024 | Raw unstandardised values",
    x        = "", y = "Value",
    caption  = "Outliers in vermillion | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    axis.text.y      = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

print(p_box_b)
cat("Plot 06b displayed. Save manually if satisfied.\n")

# ════════════════════════════════════════════════════════════════
# SECTION 15: EDA — OUTLIER HEATMAP
# Only showing companies with at least one |z| > 2
# ════════════════════════════════════════════════════════════════

outlier_long <- as.data.frame(X_A) %>%
  setNames(short_names) %>%
  mutate(Company = complete_data$Company) %>%
  pivot_longer(-Company,
               names_to  = "Indicator",
               values_to = "Z_Score")

cat("\nExtreme outliers (|z| > 3):\n")
extreme <- outlier_long %>% filter(abs(Z_Score) > 3)
if(nrow(extreme) > 0) {
  print(extreme %>% arrange(desc(abs(Z_Score))))
} else {
  cat("  No extreme outliers found.\n")
}

notable_companies <- outlier_long %>%
  group_by(Company) %>%
  summarise(max_z = max(abs(Z_Score)), .groups = "drop") %>%
  filter(max_z > 2) %>%
  pull(Company)

cat("Companies with at least one |z| > 2:", length(notable_companies), "\n")

p_outlier <- outlier_long %>%
  filter(Company %in% notable_companies) %>%
  ggplot(aes(x    = Indicator,
             y    = Company,
             fill = Z_Score)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  scale_fill_gradient2(
    low      = plot_highlight,
    mid      = "white",
    high     = plot_blue,
    midpoint = 0,
    name     = "Z-Score"
  ) +
  labs(
    title    = "Standardised Indicator Values — Companies with Notable Values (|z| > 2)",
    subtitle = "Vermillion = below average | Blue = above average",
    x        = "Indicator", y = "",
    caption  = "Only companies with at least one |z| > 2 shown | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(colour = "grey40", size = 9),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y   = element_text(size = 9),
    panel.grid    = element_blank()
  )

print(p_outlier)
cat("Plot 07 displayed. Save manually if satisfied.\n")

# ════════════════════════════════════════════════════════════════
# SECTION 16: EDA — SCATTERPLOTS (split into two plots)
# ════════════════════════════════════════════════════════════════

scatter_data <- pred_raw %>%
  mutate(OverallRank = y) %>%
  pivot_longer(-OverallRank,
               names_to  = "Indicator",
               values_to = "Value")

#Indicators 01-12
p_scatter_a <- scatter_data %>%
  filter(Indicator %in% short_names[1:12]) %>%
  ggplot(aes(x = Value, y = OverallRank)) +
  geom_point(colour = plot_blue, alpha = 0.45, size = 1.5) +
  geom_smooth(method    = "lm",
              colour    = plot_highlight,
              fill      = plot_highlight,
              se        = TRUE,
              linewidth = 0.8,
              alpha     = 0.12) +
  facet_wrap(~Indicator, scales = "free_x", ncol = 3) +
  labs(
    title    = "Employment Indicators vs Social ESG Ranking (01-12)",
    subtitle = "Each panel = one indicator | Vermillion line = linear trend | Rank 1 = best",
    x        = "Indicator Value (raw)", y = "Overall Social ESG Rank",
    caption  = "Set A: Historical averages 2018-2024 | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    strip.text       = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

print(p_scatter_a)
cat("Plot 08a displayed. Save manually if satisfied.\n")

#Indicators 13-24
p_scatter_b <- scatter_data %>%
  filter(Indicator %in% short_names[13:24]) %>%
  ggplot(aes(x = Value, y = OverallRank)) +
  geom_point(colour = plot_blue, alpha = 0.45, size = 1.5) +
  geom_smooth(method    = "lm",
              colour    = plot_highlight,
              fill      = plot_highlight,
              se        = TRUE,
              linewidth = 0.8,
              alpha     = 0.12) +
  facet_wrap(~Indicator, scales = "free_x", ncol = 3) +
  labs(
    title    = "Employment Indicators vs Social ESG Ranking (13-24)",
    subtitle = "Each panel = one indicator | Vermillion line = linear trend | Rank 1 = best",
    x        = "Indicator Value (raw)", y = "Overall Social ESG Rank",
    caption  = "Set A: Historical averages 2018-2024 | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    strip.text       = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

print(p_scatter_b)
cat("Plot 08b displayed. Save manually if satisfied.\n")

# ════════════════════════════════════════════════════════════════
# SECTION 17: EDA — SET A VS SET B COMPARISON (split into two)
# ════════════════════════════════════════════════════════════════

shared_inds <- intersect(
  gsub("_Total$", "", predictor_cols),
  gsub("_2024$",  "", pred_B)
)

setA_long <- imputed_A %>%
  select(all_of(
    predictor_cols[gsub("_Total$", "", predictor_cols) %in% shared_inds]
  )) %>%
  mutate(Company = complete_data$Company) %>%
  pivot_longer(-Company,
               names_to  = "Indicator",
               values_to = "Value") %>%
  mutate(Set       = "Set A: Historical Average (2018-2024)",
         Indicator = gsub("_Total$", "", Indicator),
         Indicator = gsub("^X\\d+_", "", Indicator))

setB_long <- imputed_B %>%
  select(all_of(
    pred_B[gsub("_2024$", "", pred_B) %in% shared_inds]
  )) %>%
  mutate(Company = complete_data$Company) %>%
  pivot_longer(-Company,
               names_to  = "Indicator",
               values_to = "Value") %>%
  mutate(Set       = "Set B: Most Recent Year (2024)",
         Indicator = gsub("_2024$",  "", Indicator),
         Indicator = gsub("^X\\d+_", "", Indicator))

compare_data      <- bind_rows(setA_long, setB_long)
shared_sorted     <- sort(unique(compare_data$Indicator))
split_pt          <- ceiling(length(shared_sorted) / 2)
inds_part1        <- shared_sorted[1:split_pt]
inds_part2        <- shared_sorted[(split_pt+1):length(shared_sorted)]

compare_plot <- function(inds, part_label) {
  compare_data %>%
    filter(Indicator %in% inds) %>%
    ggplot(aes(x = Value, fill = Set, colour = Set)) +
    geom_density(alpha = 0.35, linewidth = 0.7) +
    scale_fill_manual(values   = plot_two) +
    scale_colour_manual(values = plot_two) +
    facet_wrap(~Indicator, scales = "free", ncol = 3) +
    labs(
      title    = paste0("Historical Average vs 2024 Values (", part_label, ")"),
      subtitle = "Blue = Set A (historical) | Orange = Set B (2024 only)",
      x        = "Indicator Value", y = "Density",
      fill = "", colour = "",
      caption  = "Okabe-Ito palette"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(colour = "grey40", size = 10),
      strip.text       = element_text(face = "bold", size = 9),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
}

p_compare_a <- compare_plot(inds_part1, "Part 1")
p_compare_b <- compare_plot(inds_part2, "Part 2")

print(p_compare_a)
cat("Plot 09a displayed. Save manually if satisfied.\n")
print(p_compare_b)
cat("Plot 09b displayed. Save manually if satisfied.\n")

# ════════════════════════════════════════════════════════════════
# SECTION 18: EDA — TOP AND BOTTOM PERFORMERS
# ════════════════════════════════════════════════════════════════

performers <- complete_data %>%
  select(Company, Industry_Clean, OverallRank) %>%
  arrange(OverallRank)

top10    <- head(performers, 10) %>% mutate(Group = "Top 10 (Best)")
bottom10 <- tail(performers, 10) %>% mutate(Group = "Bottom 10 (Worst)")

cat("\n=== TOP 10 SOCIAL ESG PERFORMERS ===\n")
print(top10 %>% select(-Group))
cat("\n=== BOTTOM 10 SOCIAL ESG PERFORMERS ===\n")
print(bottom10 %>% select(-Group) %>% arrange(OverallRank))

p_performers <- bind_rows(top10, bottom10) %>%
  ggplot(aes(x    = reorder(Company, -OverallRank),
             y    = OverallRank,
             fill = Group)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = OverallRank),
            hjust  = -0.2, size = 3.5,
            colour = unname(okabe_ito["black"])) +
  scale_fill_manual(values = c(
    "Top 10 (Best)"     = plot_blue,
    "Bottom 10 (Worst)" = plot_highlight
  )) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Top and Bottom 10 Companies by Social ESG Ranking",
    subtitle = "Rank 1 = highest Social ESG performance | Rank 74 = lowest",
    x        = "", y = "Overall Social ESG Rank", fill = "",
    caption  = "Source: Sheffield ESG Employment Database (Matanle et al., 2025) | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 10),
    axis.text.y        = element_text(size = 11),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(p_performers)
cat("Plot 10 displayed. Save manually if satisfied.\n")

# ════════════════════════════════════════════════════════════════
# SECTION 19: EDA — DISCLOSURE RATES OVER TIME
# ════════════════════════════════════════════════════════════════

year_ind_cols <- names(complete_data)[
  grepl("^X0[1-9]_|^X1[0-9]_|^X2[0-4]_", names(complete_data)) &
    !grepl("_Total$", names(complete_data))
]

disclosure_data <- complete_data %>%
  select(all_of(year_ind_cols)) %>%
  summarise(across(everything(),
                   ~sum(!is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(),
               names_to  = "Column",
               values_to = "Disclosure_Rate") %>%
  mutate(
    Year      = as.integer(gsub(".*_(\\d{4})$", "\\1", Column)),
    Indicator = gsub("_\\d{4}$", "", Column),
    Indicator = gsub("^X\\d+_",  "", Indicator)
  ) %>%
  filter(!is.na(Year))

avg_disclosure <- disclosure_data %>%
  group_by(Year) %>%
  summarise(Avg_Disclosure = round(mean(Disclosure_Rate), 1),
            .groups = "drop")

cat("\n=== AVERAGE DISCLOSURE RATE BY YEAR ===\n")
print(avg_disclosure)

#Identifying the indicator with the largest post-peak disclosure decline,
#so it can be labelled directly on the plot rather than left anonymous
disclosure_decline <- disclosure_data %>%
  group_by(Indicator) %>%
  summarise(
    Peak_Rate  = max(Disclosure_Rate),
    Rate_2024  = Disclosure_Rate[Year == 2024],
    Decline    = Peak_Rate - Rate_2024,
    .groups    = "drop"
  ) %>%
  arrange(desc(Decline))

cat("=== INDICATORS WITH LARGEST DISCLOSURE DECLINE (peak to 2024) ===\n")
print(head(disclosure_decline, 5))

flagged_indicator <- disclosure_decline$Indicator[1]
cat("\nFlagged for labelling on plot:", flagged_indicator, "\n")

flagged_line <- disclosure_data %>% filter(Indicator == flagged_indicator)

p_disclosure <- ggplot(disclosure_data,
                       aes(x      = Year,
                           y      = Disclosure_Rate,
                           group  = Indicator,
                           colour = Indicator)) +
  geom_line(alpha = 0.25, linewidth = 0.6, show.legend = FALSE) +
  geom_line(data        = flagged_line,
            aes(x = Year, y = Disclosure_Rate, group = 1),
            colour      = "black",
            linewidth   = 1,
            inherit.aes = FALSE) +
  geom_line(data        = avg_disclosure,
            aes(x = Year, y = Avg_Disclosure, group = 1),
            colour      = plot_highlight,
            linewidth   = 1.5,
            inherit.aes = FALSE) +
  annotate("text",
           x      = 2024,
           y      = avg_disclosure$Avg_Disclosure[
             avg_disclosure$Year == 2024] + 3,
           label  = "Average",
           colour = plot_highlight, size = 4, hjust = 1) +
  annotate("text",
           x      = 2024,
           y      = flagged_line$Disclosure_Rate[flagged_line$Year == 2024] - 4,
           label  = flagged_indicator,
           colour = "black", size = 3.5, hjust = 1, fontface = "bold") +
  scale_colour_manual(
    values = rep(plot_full,
                 length.out = n_distinct(disclosure_data$Indicator))) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(limits = c(0, 105),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Employment Indicator Disclosure Rates Over Time",
    subtitle = "Each line = one indicator | Vermillion = average | Black = largest post-peak decline",
    x        = "Year", y = "Disclosure Rate (%)",
    caption  = "74 KOSPI-100 listed Korean firms | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    panel.grid.minor = element_blank()
  )

print(p_disclosure)
cat("Plot 11 displayed. Save manually if satisfied.\n")

# ════════════════════════════════════════════════════════════════
# SECTION 20: EDA — RQ2 SHARE PRICE DISTRIBUTION
# ════════════════════════════════════════════════════════════════

rq2_data <- complete_data %>%
  mutate(across(starts_with("Share_Price_"),
                ~ifelse(grepl("DIV/0|#N/A|#VALUE|#REF", as.character(.)),
                        NA,
                        as.numeric(gsub("[^0-9.-]", "", as.character(.)))))) %>%
  filter(!is.na(Share_Price_2018), !is.na(Share_Price_2022)) %>%
  select(Company, Industry_Clean, OverallRank,
         Share_Price_2018, Share_Price_2019,
         Share_Price_2020, Share_Price_2021,
         Share_Price_2022)

cat("\n=== RQ2 DATASET ===\n")
cat("Companies with share price data:", nrow(rq2_data), "\n")
cat("Share price 2022 summary:\n")
print(summary(rq2_data$Share_Price_2022))
cat("SD:", round(sd(rq2_data$Share_Price_2022, na.rm = TRUE), 2), "\n")
cat("Skewness:", round(skewness(rq2_data$Share_Price_2022), 3), "\n")

p_sp_dist <- ggplot(rq2_data, aes(x = Share_Price_2022)) +
  geom_histogram(binwidth = 20,
                 fill     = plot_blue,
                 colour   = "white",
                 alpha    = 0.9) +
  geom_vline(xintercept = 100,
             linetype   = "dashed",
             colour     = plot_highlight,
             linewidth  = 0.9) +
  annotate("text", x = 102, y = Inf,
           label  = "2018 baseline = 100",
           colour = plot_highlight,
           vjust  = 2, hjust = 0, size = 3.5) +
  labs(
    title    = "Distribution of Share Price Performance (December 2022)",
    subtitle = "46 KOSPI-listed firms | Indexed to 2018 baseline = 100",
    x        = "Share Price (2018 = 100)", y = "Number of Companies",
    caption  = "Source: Sheffield ESG Employment Database (Matanle et al., 2025) | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 11),
    panel.grid.minor = element_blank()
  )

print(p_sp_dist)
cat("Plot 12 displayed. Save manually if satisfied.\n")

# ════════════════════════════════════════════════════════════════
# SECTION 21: EDA — RQ2 ESG RANK VS SHARE PRICE
# ════════════════════════════════════════════════════════════════

p_rq2 <- ggplot(rq2_data,
                aes(x = OverallRank, y = Share_Price_2022)) +
  geom_point(aes(colour = Industry_Clean),
             size = 3, alpha = 0.8) +
  geom_smooth(method    = "lm",
              colour    = plot_highlight,
              fill      = plot_highlight,
              se        = TRUE,
              linewidth = 1,
              alpha     = 0.15) +
  geom_text_repel(
    data = rq2_data %>%
      filter(Share_Price_2022 > quantile(Share_Price_2022, 0.85) |
               Share_Price_2022 < quantile(Share_Price_2022, 0.15)),
    aes(label = Company),
    size = 3, colour = unname(okabe_ito["black"]),
    max.overlaps = 15
  ) +
  scale_colour_manual(
    values = rep(plot_full,
                 length.out = n_distinct(rq2_data$Industry_Clean))) +
  labs(
    title    = "Social ESG Ranking vs Share Price Performance",
    subtitle = "46 KOSPI-listed firms | Share price indexed to 2018 = 100 | Extreme companies labelled",
    x        = "Overall Social ESG Rank (1 = best)",
    y        = "Share Price December 2022 (2018 = 100)",
    colour   = "Industry",
    caption  = "Vermillion line = linear trend | Okabe-Ito colour blind friendly palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    legend.position  = "right",
    legend.text      = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

print(p_rq2)
cat("Plot 13 displayed. Save manually if satisfied.\n")

# ════════════════════════════════════════════════════════════════
# SECTION 22: EDA — SECTOR BREAKDOWN OF KEY INDICATORS
# Only sectors with 4+ companies shown
# ════════════════════════════════════════════════════════════════

key_inds <- c("FemalePermanentPct", "FemaleMiddleMgrPct",
              "FemaleExecPct",      "GenderPayGap",
              "TrainingHours",      "VoluntaryTurnover")

main_sectors <- complete_data %>%
  count(Industry_Clean) %>%
  filter(n >= 4) %>%
  pull(Industry_Clean)

sector_ind_data <- complete_data %>%
  filter(Industry_Clean %in% main_sectors) %>%
  select(Company, Industry_Clean, all_of(predictor_cols)) %>%
  pivot_longer(cols      = all_of(predictor_cols),
               names_to  = "Indicator",
               values_to = "Value") %>%
  mutate(Indicator = gsub("_Total$", "", Indicator),
         Indicator = gsub("^X\\d+_", "", Indicator)) %>%
  filter(!is.na(Value), Indicator %in% key_inds)

p_sector <- ggplot(sector_ind_data,
                   aes(x    = reorder(Industry_Clean, Value, median),
                       y    = Value,
                       fill = Industry_Clean)) +
  geom_boxplot(outlier.shape  = 21,
               outlier.fill   = plot_highlight,
               outlier.colour = "white",
               outlier.size   = 2,
               alpha          = 0.8,
               show.legend    = FALSE) +
  scale_fill_manual(
    values = rep(plot_full,
                 length.out = n_distinct(sector_ind_data$Industry_Clean))) +
  facet_wrap(~Indicator, scales = "free", ncol = 3) +
  coord_flip() +
  labs(
    title    = "Key Employment Indicators by Industry Sector",
    subtitle = paste0("Sectors with 4+ companies | ",
                      paste(main_sectors, collapse = ", ")),
    x        = "", y = "Indicator Value",
    caption  = "Outliers in vermillion | Okabe-Ito colour blind friendly palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(colour = "grey40", size = 8),
    strip.text       = element_text(face = "bold", size = 10),
    axis.text.y      = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

print(p_sector)
cat("Plot 14 displayed. Save manually if satisfied.\n")

cat("\n=== EDA COMPLETE ===\n")
cat("All plots displayed. Save any you are happy with via\n")
cat("the Export button in the RStudio Plots pane.\n")
cat("Next step: modelling.\n")


#Verifying all required objects exist before modelling
cat("=== PRE-MODELLING CHECK ===\n")
cat("X_A dimensions:", nrow(X_A), "x", ncol(X_A), "\n")
cat("X_B dimensions:", nrow(X_B), "x", ncol(X_B), "\n")
cat("y length:", length(y), "| range:", min(y), "to", max(y), "\n")
cat("predictor_cols:", length(predictor_cols), "\n")
cat("pred_B:", length(pred_B), "\n")
cat("short_names:", length(short_names), "\n")
cat("complete_data:", nrow(complete_data), "rows\n")
cat("\nAll objects present. Ready for modelling.\n")


# ════════════════════════════════════════════════════════════════
# MODEL 1: LASSO REGRESSION
# Primary model for RQ1
# alpha = 1 (Lasso penalty)
# LOOCV for lambda selection (nfolds = nrow = leave one out)
# Run on both Set A (Total) and Set B (2024)
# ════════════════════════════════════════════════════════════════

set.seed(123)

#─── SET A: Historical averages 2018-2024 ────────────────────────

lasso_cv_A <- cv.glmnet(
  x        = X_A,
  y        = y,
  alpha    = 1,
  nfolds   = nrow(X_A),
  type.measure = "mse"
)

#Extracting coefficients at best lambda
lasso_coef_A <- coef(lasso_cv_A, s = "lambda.min")
lasso_df_A <- data.frame(
  Indicator   = rownames(lasso_coef_A)[-1],
  Coefficient = as.numeric(lasso_coef_A)[-1]
) %>%
  mutate(
    Abs_Coef  = abs(Coefficient),
    Direction = ifelse(Coefficient > 0, "Positive", "Negative"),
    Set       = "Set A: Historical Average"
  ) %>%
  filter(Coefficient != 0) %>%
  arrange(desc(Abs_Coef))

cat("=== LASSO SET A RESULTS ===\n")
cat("Best lambda:", round(lasso_cv_A$lambda.min, 4), "\n")
cat("Lambda 1se:", round(lasso_cv_A$lambda.1se, 4), "\n")
cat("Non-zero coefficients:", nrow(lasso_df_A), "out of", ncol(X_A), "\n\n")
print(lasso_df_A)

#─── SET B: 2024 columns only ────────────────────────────────────

set.seed(123)

lasso_cv_B <- cv.glmnet(
  x        = X_B,
  y        = y,
  alpha    = 1,
  nfolds   = nrow(X_B),
  type.measure = "mse"
)

lasso_coef_B <- coef(lasso_cv_B, s = "lambda.min")
lasso_df_B <- data.frame(
  Indicator   = rownames(lasso_coef_B)[-1],
  Coefficient = as.numeric(lasso_coef_B)[-1]
) %>%
  mutate(
    Abs_Coef  = abs(Coefficient),
    Direction = ifelse(Coefficient > 0, "Positive", "Negative"),
    Set       = "Set B: 2024 Only"
  ) %>%
  filter(Coefficient != 0) %>%
  arrange(desc(Abs_Coef))

cat("\n=== LASSO SET B RESULTS ===\n")
cat("Best lambda:", round(lasso_cv_B$lambda.min, 4), "\n")
cat("Lambda 1se:", round(lasso_cv_B$lambda.1se, 4), "\n")
cat("Non-zero coefficients:", nrow(lasso_df_B), "out of", ncol(X_B), "\n\n")
print(lasso_df_B)

#─── COMPARING SETS A AND B ──────────────────────────────────────

# Clean indicator names for accurate comparison across Set A and Set B
clean_A <- gsub("_Total$", "", lasso_df_A$Indicator)
clean_A <- gsub("^X\\d+_", "", clean_A)

clean_B <- gsub("_2024$", "", lasso_df_B$Indicator)
clean_B <- gsub("^X\\d+_", "", clean_B)

cat("\n=== LASSO COMPARISON: SET A vs SET B ===\n")
cat("Indicators surviving in BOTH sets:\n")
both_lasso <- intersect(clean_A, clean_B)
print(both_lasso)

cat("\nIndicators in Set A only:\n")
print(setdiff(clean_A, clean_B))

cat("\nIndicators in Set B only:\n")
print(setdiff(clean_B, clean_A))

#─── VISUALISING LASSO COEFFICIENTS ──────────────────────────────

#Combining both sets for side by side plot
lasso_combined <- bind_rows(lasso_df_A, lasso_df_B) %>%
  mutate(Indicator = gsub("_Total$|_2024$", "", Indicator),
         Indicator = gsub("^X\\d+_", "", Indicator))

p_lasso <- ggplot(lasso_combined,
                  aes(x    = reorder(Indicator, Abs_Coef),
                      y    = Coefficient,
                      fill = Direction)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 0,
             colour     = unname(okabe_ito["black"]),
             linewidth  = 0.5) +
  scale_fill_manual(values = c(
    "Positive" = plot_blue,
    "Negative" = plot_highlight
  )) +
  facet_wrap(~Set, scales = "free_y") +
  coord_flip() +
  labs(
    title    = "Lasso Regression: Non-Zero Coefficients by Model Set",
    subtitle = "Blue = positive association with rank (worse) | Vermillion = negative (better)",
    x        = "", y = "Coefficient",
    fill     = "Direction",
    caption  = "LOOCV lambda selection | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(colour = "grey40", size = 9),
    strip.text       = element_text(face = "bold", size = 10),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92")
  )

print(p_lasso)
cat("\nLasso complete. Save plot manually if satisfied.\n")
cat("Next step: Elastic Net.\n")


# ════════════════════════════════════════════════════════════════
# MODEL 2: ELASTIC NET REGRESSION
# Primary model for RQ1
# alpha = 0.5 (equal Lasso and Ridge penalty)
# LOOCV for lambda selection
# Run on both Set A and Set B
# ════════════════════════════════════════════════════════════════

set.seed(123)

#─── SET A: Historical averages 2018-2024 ────────────────────────

enet_cv_A <- cv.glmnet(
  x            = X_A,
  y            = y,
  alpha        = 0.5,
  nfolds       = nrow(X_A),
  type.measure = "mse"
)

enet_coef_A <- coef(enet_cv_A, s = "lambda.min")
enet_df_A <- data.frame(
  Indicator   = rownames(enet_coef_A)[-1],
  Coefficient = as.numeric(enet_coef_A)[-1]
) %>%
  mutate(
    Abs_Coef  = abs(Coefficient),
    Direction = ifelse(Coefficient > 0, "Positive", "Negative"),
    Set       = "Set A: Historical Average"
  ) %>%
  filter(Coefficient != 0) %>%
  arrange(desc(Abs_Coef))

cat("=== ELASTIC NET SET A RESULTS ===\n")
cat("Best lambda:", round(enet_cv_A$lambda.min, 4), "\n")
cat("Non-zero coefficients:", nrow(enet_df_A), "out of", ncol(X_A), "\n\n")
print(enet_df_A)

#─── SET B: 2024 columns only ────────────────────────────────────

set.seed(123)

enet_cv_B <- cv.glmnet(
  x            = X_B,
  y            = y,
  alpha        = 0.5,
  nfolds       = nrow(X_B),
  type.measure = "mse"
)

enet_coef_B <- coef(enet_cv_B, s = "lambda.min")
enet_df_B <- data.frame(
  Indicator   = rownames(enet_coef_B)[-1],
  Coefficient = as.numeric(enet_coef_B)[-1]
) %>%
  mutate(
    Abs_Coef  = abs(Coefficient),
    Direction = ifelse(Coefficient > 0, "Positive", "Negative"),
    Set       = "Set B: 2024 Only"
  ) %>%
  filter(Coefficient != 0) %>%
  arrange(desc(Abs_Coef))

cat("\n=== ELASTIC NET SET B RESULTS ===\n")
cat("Best lambda:", round(enet_cv_B$lambda.min, 4), "\n")
cat("Non-zero coefficients:", nrow(enet_df_B), "out of", ncol(X_B), "\n\n")
print(enet_df_B)

#─── COMPARING SETS A AND B (stripping suffixes for fair comparison) ──

#Creating clean indicator names without _Total or _2024 suffixes
enet_df_A_clean <- enet_df_A %>%
  mutate(Indicator_Clean = gsub("_Total$|_2024$", "", Indicator),
         Indicator_Clean = gsub("^X\\d+_", "", Indicator_Clean))

enet_df_B_clean <- enet_df_B %>%
  mutate(Indicator_Clean = gsub("_Total$|_2024$", "", Indicator),
         Indicator_Clean = gsub("^X\\d+_", "", Indicator_Clean))

cat("\n=== ELASTIC NET COMPARISON: SET A vs SET B ===\n")
cat("Indicators surviving in BOTH sets:\n")
both_enet <- intersect(enet_df_A_clean$Indicator_Clean,
                       enet_df_B_clean$Indicator_Clean)
print(both_enet)
cat("\nIndicators in Set A only:\n")
print(setdiff(enet_df_A_clean$Indicator_Clean,
              enet_df_B_clean$Indicator_Clean))
cat("\nIndicators in Set B only:\n")
print(setdiff(enet_df_B_clean$Indicator_Clean,
              enet_df_A_clean$Indicator_Clean))

#─── COMPARING LASSO vs ELASTIC NET (Set A) ──────────────────────

lasso_df_A_clean <- lasso_df_A %>%
  mutate(Indicator_Clean = gsub("_Total$|_2024$", "", Indicator),
         Indicator_Clean = gsub("^X\\d+_", "", Indicator_Clean))

cat("\n=== LASSO vs ELASTIC NET (Set A) ===\n")
cat("Indicators surviving in BOTH Lasso and Elastic Net (Set A):\n")
both_A <- intersect(lasso_df_A_clean$Indicator_Clean,
                    enet_df_A_clean$Indicator_Clean)
print(both_A)
cat("\nIn Lasso only:\n")
print(setdiff(lasso_df_A_clean$Indicator_Clean,
              enet_df_A_clean$Indicator_Clean))
cat("\nIn Elastic Net only:\n")
print(setdiff(enet_df_A_clean$Indicator_Clean,
              lasso_df_A_clean$Indicator_Clean))

#─── VISUALISING ELASTIC NET COEFFICIENTS ────────────────────────

enet_combined <- bind_rows(enet_df_A, enet_df_B) %>%
  mutate(Indicator = gsub("_Total$|_2024$", "", Indicator),
         Indicator = gsub("^X\\d+_", "", Indicator))

p_enet <- ggplot(enet_combined,
                 aes(x    = reorder(Indicator, Abs_Coef),
                     y    = Coefficient,
                     fill = Direction)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 0,
             colour     = unname(okabe_ito["black"]),
             linewidth  = 0.5) +
  scale_fill_manual(values = c(
    "Positive" = plot_blue,
    "Negative" = plot_highlight
  )) +
  facet_wrap(~Set, scales = "free_y") +
  coord_flip() +
  labs(
    title    = "Elastic Net Regression: Non-Zero Coefficients by Model Set",
    subtitle = "Blue = positive association with rank (worse) | Vermillion = negative (better)",
    x        = "", y = "Coefficient",
    fill     = "Direction",
    caption  = "LOOCV lambda selection | alpha = 0.5 | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    strip.text         = element_text(face = "bold", size = 10),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(p_enet)
cat("\nElastic Net complete. Save plot manually if satisfied.\n")
cat("Next step: Random Forest.\n")


# ════════════════════════════════════════════════════════════════
# MODEL 3: RANDOM FOREST (WITH HYPERPARAMETER TUNING)
# Robustness check for RQ1
# Permutation importance (not impurity — avoids inflating
# continuous variables, following Strobl et al. 2007)
# mtry and min.node.size now tuned via OOB error (free with ranger,
# no extra cross-validation needed)
# Run on both Set A and Set B
# ════════════════════════════════════════════════════════════════

tune_rf <- function(X, y, short_names, set_label) {
  rf_data <- as.data.frame(X)
  colnames(rf_data) <- short_names
  rf_data$outcome   <- y
  
  rf_grid <- unique(expand.grid(
    mtry          = c(2, 4, 6, 8, round(sqrt(ncol(X))), round(ncol(X) / 3)),
    min.node.size = c(1, 3, 5)
  ))
  
  cat("=== RANDOM FOREST HYPERPARAMETER TUNING (", set_label, ") ===\n", sep = "")
  results <- data.frame()
  for (i in seq_len(nrow(rf_grid))) {
    set.seed(123)   # <-- seed set before every grid combination, same as your original pattern
    m <- ranger(
      outcome    ~ .,
      data       = rf_data,
      num.trees  = 500,
      mtry       = rf_grid$mtry[i],
      min.node.size = rf_grid$min.node.size[i],
      importance = "permutation",
      seed       = 123
    )
    results <- rbind(results, data.frame(
      mtry = rf_grid$mtry[i], min.node.size = rf_grid$min.node.size[i],
      OOB_RMSE = sqrt(m$prediction.error), OOB_R2 = m$r.squared
    ))
  }
  results <- results[order(results$OOB_RMSE), ]
  cat("Full tuning grid, best first:\n")
  print(results)
  
  best <- results[1, ]
  cat("Best combination: mtry =", best$mtry, "| min.node.size =", best$min.node.size,
      "| OOB RMSE =", round(best$OOB_RMSE, 3), "\n")
  
  set.seed(123)   # <-- seed set again before refitting the final chosen model
  final_model <- ranger(
    outcome    ~ .,
    data       = rf_data,
    num.trees  = 500,
    mtry       = best$mtry,
    min.node.size = best$min.node.size,
    importance = "permutation",
    seed       = 123
  )
  imp <- data.frame(
    Indicator  = names(final_model$variable.importance),
    Importance = final_model$variable.importance,
    Set        = set_label
  ) %>% arrange(desc(Importance))
  
  cat("=== FINAL TUNED RANDOM FOREST (", set_label, ") ===\n", sep = "")
  cat("R-squared (OOB):", round(final_model$r.squared, 3), "\n")
  cat("RMSE (OOB):", round(sqrt(final_model$prediction.error), 3), "\n")
  cat("Top 10 indicators by permutation importance:\n")
  print(head(imp, 10))
  
  list(model = final_model, importance = imp)
}

#─── SET A: Historical averages 2018-2024 ────────────────────────

rf_result_A <- tune_rf(X_A, y, short_names, "Set A: Historical Average")
rf_model_A  <- rf_result_A$model
rf_imp_A    <- rf_result_A$importance

#─── SET B: 2024 columns only ────────────────────────────────────

short_names_B <- gsub("_2024$", "", pred_B)
short_names_B <- gsub("^X\\d+_", "", short_names_B)

rf_result_B <- tune_rf(X_B, y, short_names_B, "Set B: 2024 Only")
rf_model_B  <- rf_result_B$model
rf_imp_B    <- rf_result_B$importance


#─── COMPARING SETS A AND B ──────────────────────────────────────

cat("\n=== RF COMPARISON: SET A vs SET B ===\n")
cat("Top 10 indicators in BOTH sets:\n")
top10_A <- head(rf_imp_A, 10)$Indicator
top10_B <- head(rf_imp_B, 10)$Indicator
both_rf  <- intersect(top10_A, top10_B)
print(both_rf)

#─── COMPARING RF vs REGULARISED REGRESSION (Set A) ─────────────

cat("\n=== RF vs LASSO/ENET (Set A) ===\n")
cat("Indicators in top 10 RF AND surviving Lasso/Enet (Set A):\n")
robust_reg <- lasso_df_A_clean$Indicator_Clean
robust_rf  <- head(rf_imp_A, 10)$Indicator
print(intersect(robust_rf, robust_reg))

#─── VISUALISING RF IMPORTANCE ───────────────────────────────────

rf_combined <- bind_rows(
  head(rf_imp_A, 15),
  head(rf_imp_B, 15)
)

p_rf <- ggplot(rf_combined,
               aes(x    = reorder(Indicator, Importance),
                   y    = Importance,
                   fill = Set)) +
  geom_col(width = 0.7, alpha = 0.9, show.legend = FALSE) +
  scale_fill_manual(values = plot_two) +
  facet_wrap(~Set, scales = "free") +
  coord_flip() +
  labs(
    title    = "Random Forest: Permutation Importance by Model Set",
    subtitle = "Top 15 indicators shown | Permutation importance (Strobl et al., 2007)",
    x        = "", y = "Permutation Importance",
    caption  = "500 trees | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    strip.text         = element_text(face = "bold", size = 10),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(p_rf)
cat("\nRandom Forest complete. Save plot manually if satisfied.\n")
cat("Next step: XGBoost.\n")


# ════════════════════════════════════════════════════════════════
# MODEL 4: XGBOOST (WITH HYPERPARAMETER TUNING)
# Robustness check for RQ1
# Gain importance used
# max_depth, eta, subsample tuned via 10-fold CV; nrounds picked via
# early stopping within xgb.cv. 10-fold (not full LOOCV) used for
# tuning to keep runtime reasonable -- your existing manual LOOCV
# loop below still provides the final, honest performance evaluation
# once these tuned hyperparameters are fixed.
# Run on both Set A and Set B
# ════════════════════════════════════════════════════════════════

tune_xgb <- function(X, y, short_names, set_label) {
  dtrain <- xgb.DMatrix(data = X, label = as.numeric(y))
  
  xgb_grid <- expand.grid(
    max_depth = c(2, 3, 4),
    eta       = c(0.05, 0.1, 0.2),
    subsample = c(0.7, 0.8, 1.0)
  )
  
  cat("=== XGBOOST HYPERPARAMETER TUNING (", set_label, ") ===\n", sep = "")
  results <- data.frame()
  for (i in seq_len(nrow(xgb_grid))) {
    set.seed(123)   # <-- seed set before every grid combination
    params_i <- list(
      objective = "reg:squarederror",
      max_depth = xgb_grid$max_depth[i],
      eta       = xgb_grid$eta[i],
      subsample = xgb_grid$subsample[i]
    )
    cv_i <- xgb.cv(
      params                = params_i,
      data                  = dtrain,
      nrounds               = 500,
      nfold                 = 10,
      early_stopping_rounds = 20,
      verbose               = 0
    )
    # Best round taken directly from the evaluation log rather than
    # cv_i$best_iteration, since that attribute's location has moved
    # across xgboost package versions -- this works regardless of
    # which version you have installed.
    best_iter <- which.min(cv_i$evaluation_log$test_rmse_mean)
    best_rmse <- cv_i$evaluation_log$test_rmse_mean[best_iter]
    
    results <- rbind(results, data.frame(
      max_depth = xgb_grid$max_depth[i], eta = xgb_grid$eta[i],
      subsample = xgb_grid$subsample[i], best_nrounds = best_iter, CV_RMSE = best_rmse
    ))
  }
  results <- results[order(results$CV_RMSE), ]
  cat("Full tuning grid, best first (top 10 shown):\n")
  print(head(results, 10))
  
  best <- results[1, ]
  cat("Best combination: max_depth =", best$max_depth, "| eta =", best$eta,
      "| subsample =", best$subsample, "| nrounds =", best$best_nrounds,
      "| 10-fold CV RMSE =", round(best$CV_RMSE, 3), "\n")
  
  final_params <- list(
    objective = "reg:squarederror",
    max_depth = best$max_depth,
    eta       = best$eta,
    subsample = best$subsample
  )
  
  set.seed(123)   # <-- seed set again before the final refit
  final_model <- xgb.train(
    params  = final_params,
    data    = dtrain,
    nrounds = best$best_nrounds,
    verbose = 0
  )
  
  imp <- xgb.importance(
    feature_names = short_names,
    model         = final_model
  ) %>%
    as.data.frame() %>%
    mutate(Set = set_label) %>%
    arrange(desc(Gain))
  
  cat("=== FINAL TUNED XGBOOST (", set_label, ") ===\n", sep = "")
  cat("Top 10 indicators by gain importance:\n")
  print(head(imp, 10))
  
  list(model = final_model, importance = imp, params = final_params, nrounds = best$best_nrounds)
}

#─── SET A: Historical averages 2018-2024 ────────────────────────

xgb_result_A <- tune_xgb(X_A, y, short_names, "Set A: Historical Average")
xgb_model_A  <- xgb_result_A$model
xgb_imp_A    <- xgb_result_A$importance
params_A     <- xgb_result_A$params   # used later in your LOOCV loop
nrounds_A    <- xgb_result_A$nrounds  # used later in your LOOCV loop

#─── SET B: 2024 columns only ────────────────────────────────────

xgb_result_B <- tune_xgb(X_B, y, short_names_B, "Set B: 2024 Only")
xgb_model_B  <- xgb_result_B$model
xgb_imp_B    <- xgb_result_B$importance
params_B     <- xgb_result_B$params   # used later in your LOOCV loop
nrounds_B    <- xgb_result_B$nrounds  # used later in your LOOCV loop


#─── COMPARING SETS A AND B ──────────────────────────────────────

cat("\n=== XGBOOST COMPARISON: SET A vs SET B ===\n")
cat("Top 10 indicators in BOTH sets:\n")
top10_xgb_A <- head(xgb_imp_A, 10)$Feature
top10_xgb_B <- head(xgb_imp_B, 10)$Feature
both_xgb    <- intersect(top10_xgb_A, top10_xgb_B)
print(both_xgb)

#─── COMPARING ALL FOUR MODELS (Set A) ───────────────────────────

cat("\n=== ALL FOUR MODELS: ROBUST INDICATORS (Set A) ===\n")
cat("Appearing in ALL four models:\n")
robust_all <- Reduce(intersect, list(
  lasso_df_A_clean$Indicator_Clean,
  enet_df_A_clean$Indicator_Clean,
  head(rf_imp_A, 10)$Indicator,
  head(xgb_imp_A, 10)$Feature
))
print(robust_all)

cat("\nAppearing in at least 3 of 4 models:\n")
all_indicators <- unique(c(
  lasso_df_A_clean$Indicator_Clean,
  enet_df_A_clean$Indicator_Clean,
  head(rf_imp_A, 10)$Indicator,
  head(xgb_imp_A, 10)$Feature
))
count_models <- sapply(all_indicators, function(ind) {
  sum(c(
    ind %in% lasso_df_A_clean$Indicator_Clean,
    ind %in% enet_df_A_clean$Indicator_Clean,
    ind %in% head(rf_imp_A, 10)$Indicator,
    ind %in% head(xgb_imp_A, 10)$Feature
  ))
})
robust_3of4 <- names(count_models[count_models >= 3])
print(sort(robust_3of4))

#─── VISUALISING XGBOOST IMPORTANCE ──────────────────────────────

xgb_combined <- bind_rows(
  head(xgb_imp_A, 15),
  head(xgb_imp_B, 15)
)

p_xgb <- ggplot(xgb_combined,
                aes(x    = reorder(Feature, Gain),
                    y    = Gain,
                    fill = Set)) +
  geom_col(width = 0.7, alpha = 0.9, show.legend = FALSE) +
  scale_fill_manual(values = plot_two) +
  facet_wrap(~Set, scales = "free") +
  coord_flip() +
  labs(
    title    = "XGBoost: Gain Importance by Model Set",
    subtitle = "Top 15 indicators shown | Gain = average improvement in accuracy per split",
    x        = "", y = "Gain",
    caption  = "Hyperparameters tuned per predictor set via 10-fold CV (see console output) | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    strip.text         = element_text(face = "bold", size = 10),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(p_xgb)
cat("\nXGBoost complete. Save plot manually if satisfied.\n")
cat("Next step: Cross-model importance comparison.\n")


# ════════════════════════════════════════════════════════════════
# CROSS-VALIDATION: PERFORMANCE ESTIMATES FOR ALL FOUR MODELS
# Lasso and EN: LOOCV already done via cv.glmnet
# RF: OOB already computed
# XGBoost: LOOCV added
# Additional: LOOCV RMSE for Lasso and EN extracted for comparison
# ════════════════════════════════════════════════════════════════

#─── LASSO LOOCV RMSE (Set A and B) ─────────────────────────────

lasso_rmse_A <- sqrt(min(lasso_cv_A$cvm))
lasso_rmse_B <- sqrt(min(lasso_cv_B$cvm))

cat("=== LASSO LOOCV RMSE ===\n")
cat("Set A:", round(lasso_rmse_A, 3), "\n")
cat("Set B:", round(lasso_rmse_B, 3), "\n")

#─── ELASTIC NET LOOCV RMSE (Set A and B) ────────────────────────

enet_rmse_A <- sqrt(min(enet_cv_A$cvm))
enet_rmse_B <- sqrt(min(enet_cv_B$cvm))

cat("\n=== ELASTIC NET LOOCV RMSE ===\n")
cat("Set A:", round(enet_rmse_A, 3), "\n")
cat("Set B:", round(enet_rmse_B, 3), "\n")

#─── RANDOM FOREST OOB RMSE (already computed) ───────────────────

rf_rmse_A <- round(sqrt(rf_model_A$prediction.error), 3)
rf_rmse_B <- round(sqrt(rf_model_B$prediction.error), 3)
rf_r2_A   <- round(rf_model_A$r.squared, 3)
rf_r2_B   <- round(rf_model_B$r.squared, 3)

cat("\n=== RANDOM FOREST OOB RMSE ===\n")
cat("Set A:", rf_rmse_A, "| R-squared:", rf_r2_A, "\n")
cat("Set B:", rf_rmse_B, "| R-squared:", rf_r2_B, "\n")

#─── XGBOOST LOOCV ───────────────────────────────────────────────

set.seed(123)

#LOOCV for XGBoost Set A
n         <- nrow(X_A)
xgb_preds_A <- numeric(n)

for(i in 1:n) {
  train_x <- X_A[-i, , drop = FALSE]
  train_y <- y[-i]
  test_x  <- X_A[i, , drop = FALSE]
  
  dtrain <- xgb.DMatrix(data = train_x, label = as.numeric(train_y))
  dtest  <- xgb.DMatrix(data = test_x)
  
  model_i <- xgb.train(
    params  = params_A,
    data    = dtrain,
    nrounds = nrounds_A,
    verbose = 0
  )
  xgb_preds_A[i] <- predict(model_i, dtest)
}

xgb_rmse_A <- sqrt(mean((y - xgb_preds_A)^2))
xgb_r2_A   <- 1 - sum((y - xgb_preds_A)^2) / sum((y - mean(y))^2)

cat("\n=== XGBOOST LOOCV (Set A) ===\n")
cat("RMSE:", round(xgb_rmse_A, 3), "\n")
cat("R-squared:", round(xgb_r2_A, 3), "\n")
cat("(This will take a minute to run — 74 iterations)\n")

#LOOCV for XGBoost Set B
set.seed(123)
xgb_preds_B <- numeric(n)

for(i in 1:n) {
  train_x <- X_B[-i, , drop = FALSE]
  train_y <- y[-i]
  test_x  <- X_B[i, , drop = FALSE]
  
  dtrain <- xgb.DMatrix(data = train_x, label = as.numeric(train_y))
  dtest  <- xgb.DMatrix(data = test_x)
  
  model_i <- xgb.train(
    params  = params_B,
    data    = dtrain,
    nrounds = nrounds_B,
    verbose = 0
  )
  xgb_preds_B[i] <- predict(model_i, dtest)
}

xgb_rmse_B <- sqrt(mean((y - xgb_preds_B)^2))
xgb_r2_B   <- 1 - sum((y - xgb_preds_B)^2) / sum((y - mean(y))^2)

cat("\n=== XGBOOST LOOCV (Set B) ===\n")
cat("RMSE:", round(xgb_rmse_B, 3), "\n")
cat("R-squared:", round(xgb_r2_B, 3), "\n")


# ════════════════════════════════════════════════════════════════
# SET B CHECKS: missingness, descriptives, multicollinearity, outliers
# ════════════════════════════════════════════════════════════════

# ─── 1. SET B MISSINGNESS (full detail, pre-imputation) ──────────

cat("=== SET B MISSINGNESS: PER-INDICATOR ===\n")
miss_summary_B <- complete_data %>%
  select(all_of(pred_B)) %>%
  summarise(across(everything(),
                   ~round(mean(is.na(.)) * 100, 1))) %>%
  pivot_longer(everything(),
               names_to  = "Indicator",
               values_to = "Pct_Missing") %>%
  arrange(desc(Pct_Missing))
print(miss_summary_B %>% filter(Pct_Missing > 0))

cat("\n=== SET B MISSINGNESS: PER-COMPANY ===\n")
miss_by_company_B <- complete_data %>%
  select(Company, all_of(pred_B)) %>%
  mutate(n_missing = rowSums(is.na(select(., all_of(pred_B))))) %>%
  filter(n_missing > 0) %>%
  select(Company, n_missing) %>%
  arrange(desc(n_missing))
print(miss_by_company_B)

cat("\nTotal missing cells:", sum(miss_by_company_B$n_missing),
    "out of", nrow(complete_data) * length(pred_B),
    sprintf("(%.2f%%)\n", 100 * sum(miss_by_company_B$n_missing) /
              (nrow(complete_data) * length(pred_B))))
cat("Companies affected:", nrow(miss_by_company_B), "\n")

# ─── 2. SET B DESCRIPTIVE STATISTICS (skew, kurtosis) ─────────────

pred_raw_B <- imputed_B
colnames(pred_raw_B) <- short_names_B

cat("\n=== SET B PREDICTOR SUMMARY (raw, unstandardised) ===\n")
pred_desc_B <- describe(pred_raw_B) %>%
  select(n, mean, sd, median, min, max, skew, kurtosis) %>%
  round(2)
print(pred_desc_B)

# ─── 3. SET B MULTICOLLINEARITY ───────────────────────────────────

cor_matrix_B <- cor(imputed_B, use = "complete.obs")
rownames(cor_matrix_B) <- short_names_B
colnames(cor_matrix_B) <- short_names_B

cat("\n=== SET B: HIGHLY CORRELATED PAIRS (|r| > 0.7) ===\n")
cor_upper_B <- cor_matrix_B
cor_upper_B[lower.tri(cor_upper_B, diag = TRUE)] <- NA
high_cor_B  <- which(abs(cor_upper_B) > 0.7, arr.ind = TRUE)

if (nrow(high_cor_B) == 0) {
  cat("  No pairs above 0.7 found.\n")
} else {
  for (i in seq_len(nrow(high_cor_B))) {
    r1 <- rownames(cor_matrix_B)[high_cor_B[i, 1]]
    r2 <- colnames(cor_matrix_B)[high_cor_B[i, 2]]
    cat(" ", r1, "vs", r2, ":",
        round(cor_upper_B[high_cor_B[i, 1], high_cor_B[i, 2]], 3), "\n")
  }
}

p_corr_B <- ggcorrplot(
  cor_matrix_B,
  method   = "square",
  type     = "lower",
  lab      = TRUE,
  lab_size = 2.5,
  colors   = c(plot_highlight, "white", plot_blue),
  title    = "Correlation Matrix: Employment Indicators (Set B — 2024 Only)",
  ggtheme  = theme_minimal(base_size = 9)
) +
  theme(
    plot.title  = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8)
  )
print(p_corr_B)

png("04b_correlation_matrix_setB.png", width = 1400, height = 1200, res = 150)
print(p_corr_B)
dev.off()
cat("Saved: 04b_correlation_matrix_setB.png\n")

# ─── 4. SET B OUTLIERS ─────────────────────────────────────────────

outlier_long_B <- as.data.frame(X_B) %>%
  setNames(short_names_B) %>%
  mutate(Company = complete_data$Company) %>%
  pivot_longer(-Company, names_to = "Indicator", values_to = "Z_Score")

cat("\n=== SET B: EXTREME OUTLIERS (|z| > 3) ===\n")
extreme_B <- outlier_long_B %>% filter(abs(Z_Score) > 3)
if (nrow(extreme_B) > 0) {
  print(extreme_B %>% arrange(desc(abs(Z_Score))))
} else {
  cat("  No extreme outliers found.\n")
}

notable_companies_B <- outlier_long_B %>%
  group_by(Company) %>%
  summarise(max_z = max(abs(Z_Score)), .groups = "drop") %>%
  filter(max_z > 2) %>%
  pull(Company)

cat("Companies with at least one |z| > 2 in Set B:",
    length(notable_companies_B), "\n")

# ─── 5. EMPIRICAL CHECK: does missingness relate to prediction error? ──

cat("\n=== SET A vs SET B PREDICTION ERROR: MOST-MISSING SET B FIRMS ===\n")

flagged_firms <- c("Netmarble", "LG Corporation")

error_compare <- data.frame(
  Company        = complete_data$Company,
  Actual         = y,
  RF_SetA_Pred   = rf_model_A$predictions,
  RF_SetB_Pred   = rf_model_B$predictions,
  XGB_SetA_Pred  = xgb_preds_A,
  XGB_SetB_Pred  = xgb_preds_B
) %>%
  mutate(
    RF_SetA_AbsErr  = abs(Actual - RF_SetA_Pred),
    RF_SetB_AbsErr  = abs(Actual - RF_SetB_Pred),
    XGB_SetA_AbsErr = abs(Actual - XGB_SetA_Pred),
    XGB_SetB_AbsErr = abs(Actual - XGB_SetB_Pred)
  )

cat("\nFor the two most Set-B-missing firms (Netmarble, LG Corporation):\n")
print(error_compare %>%
        filter(Company %in% flagged_firms) %>%
        select(Company, Actual, RF_SetA_AbsErr, RF_SetB_AbsErr,
               XGB_SetA_AbsErr, XGB_SetB_AbsErr))

cat("\nMean absolute error, flagged firms vs all other firms (RF):\n")
error_compare %>%
  mutate(Flagged = Company %in% flagged_firms) %>%
  group_by(Flagged) %>%
  summarise(
    Mean_SetA_AbsErr = mean(RF_SetA_AbsErr),
    Mean_SetB_AbsErr = mean(RF_SetB_AbsErr),
    .groups = "drop"
  ) %>%
  print()

cat("\nMean absolute error, flagged firms vs all other firms (XGBoost):\n")
error_compare %>%
  mutate(Flagged = Company %in% flagged_firms) %>%
  group_by(Flagged) %>%
  summarise(
    Mean_SetA_AbsErr = mean(XGB_SetA_AbsErr),
    Mean_SetB_AbsErr = mean(XGB_SetB_AbsErr),
    .groups = "drop"
  ) %>%
  print()


#─── COMPLETE PERFORMANCE COMPARISON TABLE ───────────────────────

performance_df <- data.frame(
  Model = c("Lasso","Lasso",
            "Elastic Net","Elastic Net",
            "Random Forest","Random Forest",
            "XGBoost","XGBoost"),
  Set = c("Set A","Set B","Set A","Set B",
          "Set A","Set B","Set A","Set B"),
  Validation = c("LOOCV","LOOCV","LOOCV","LOOCV",
                 "OOB","OOB","LOOCV","LOOCV"),
  RMSE = c(
    round(lasso_rmse_A, 3), round(lasso_rmse_B, 3),
    round(enet_rmse_A, 3),  round(enet_rmse_B, 3),
    rf_rmse_A,              rf_rmse_B,
    round(xgb_rmse_A, 3),   round(xgb_rmse_B, 3)
  ),
  R_Squared = c(
    NA, NA, NA, NA,
    rf_r2_A, rf_r2_B,
    round(xgb_r2_A, 3), round(xgb_r2_B, 3)
  )
)

cat("\n=== COMPLETE PERFORMANCE COMPARISON ===\n")
print(performance_df)

#─── VISUALISING PERFORMANCE COMPARISON ──────────────────────────

p_performance <- ggplot(performance_df,
                        aes(x    = Model,
                            y    = RMSE,
                            fill = Set,
                            group = Set)) +
  geom_col(position = "dodge", width = 0.6, alpha = 0.9) +
  scale_fill_manual(values = plot_two) +
  labs(
    title    = "Model Performance Comparison: RMSE by Model and Predictor Set",
    subtitle = "Lower RMSE = better predictive accuracy | LOOCV for all except RF (OOB)",
    x        = "", y = "RMSE",
    fill     = "",
    caption  = "Set A = historical averages 2018-2024 | Set B = 2024 only | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

png("model_performance_comparison.png",
    width = 1200, height = 700, res = 150)
print(p_performance)
dev.off()
cat("Saved: model_performance_comparison.png\n")
cat("\nCross-validation complete.\n")

# ════════════════════════════════════════════════════════════════
# CROSS-MODEL IMPORTANCE COMPARISON
# Summarising which indicators are robust across all 4 models
# ════════════════════════════════════════════════════════════════

#Creating a summary table of model counts per indicator
model_count_df <- data.frame(
  Indicator    = names(count_models),
  Models_Count = as.numeric(count_models)
) %>%
  arrange(desc(Models_Count)) %>%
  mutate(
    Robustness = case_when(
      Models_Count == 4 ~ "All 4 models",
      Models_Count == 3 ~ "3 of 4 models",
      Models_Count == 2 ~ "2 of 4 models",
      TRUE              ~ "1 model only"
    )
  )

cat("=== CROSS-MODEL IMPORTANCE SUMMARY (Set A) ===\n")
print(model_count_df)

#Visualising cross-model agreement
p_cross <- ggplot(model_count_df,
                  aes(x    = reorder(Indicator, Models_Count),
                      y    = Models_Count,
                      fill = Robustness)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_hline(yintercept = 3,
             linetype   = "dashed",
             colour     = unname(okabe_ito["black"]),
             linewidth  = 0.6) +
  annotate("text", x = 1, y = 3.1,
           label  = "3+ model threshold",
           hjust  = 0, size = 3.5,
           colour = "grey40") +
  scale_fill_manual(values = c(
    "All 4 models"  = plot_blue,
    "3 of 4 models" = unname(okabe_ito["sky_blue"]),
    "2 of 4 models" = unname(okabe_ito["orange"]),
    "1 model only"  = "grey80"
  )) +
  scale_y_continuous(breaks = 1:4) +
  coord_flip() +
  labs(
    title    = "Cross-Model Feature Importance Agreement (Set A)",
    subtitle = "Consensus across Lasso, Elastic Net, Random Forest, and XGBoost",
    x        = "Social ESG Indicator / Feature", 
    y        = "Number of Selecting Models (Out of 4)",
    fill     = "Robustness",
    caption  = "Models: Lasso, Elastic Net, Random Forest, XGBoost | Set A: Historical averages 2018-2024"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(colour = "grey40", size = 9),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(p_cross)
cat("Cross-model comparison plot displayed.\n")
cat("Save manually if satisfied.\n")
cat("Next step: RQ2 regression.\n")


# ════════════════════════════════════════════════════════════════
# FEATURE IMPORTANCE COMPARISON: SET A vs SET B
# Side by side for all four models
# ════════════════════════════════════════════════════════════════

#─── LASSO COMPARISON ────────────────────────────────────────────

lasso_both <- bind_rows(
  lasso_df_A %>%
    mutate(Indicator = gsub("_Total$", "", Indicator),
           Indicator = gsub("^X\\d+_", "", Indicator)),
  lasso_df_B %>%
    mutate(Indicator = gsub("_2024$", "", Indicator),
           Indicator = gsub("^X\\d+_", "", Indicator))
) %>%
  group_by(Set) %>%
  mutate(Rank = row_number()) %>%
  ungroup()

p_lasso_compare <- ggplot(lasso_both,
                          aes(x    = reorder(Indicator, -Abs_Coef),
                              y    = Abs_Coef,
                              fill = Set)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = plot_two) +
  coord_flip() +
  labs(
    title    = "Lasso: Absolute Coefficients — Set A vs Set B",
    subtitle = "Blue = historical average (2018-2024) | Orange = 2024 only",
    x        = "", y = "Absolute Coefficient",
    fill     = "",
    caption  = "Only non-zero coefficients shown | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(face = "bold", size = 11),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

#─── ELASTIC NET COMPARISON ──────────────────────────────────────

enet_both <- bind_rows(
  enet_df_A %>%
    mutate(Indicator = gsub("_Total$", "", Indicator),
           Indicator = gsub("^X\\d+_", "", Indicator)),
  enet_df_B %>%
    mutate(Indicator = gsub("_2024$", "", Indicator),
           Indicator = gsub("^X\\d+_", "", Indicator))
)

p_enet_compare <- ggplot(enet_both,
                         aes(x    = reorder(Indicator, -Abs_Coef),
                             y    = Abs_Coef,
                             fill = Set)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = plot_two) +
  coord_flip() +
  labs(
    title    = "Elastic Net: Absolute Coefficients — Set A vs Set B",
    subtitle = "Blue = historical average (2018-2024) | Orange = 2024 only",
    x        = "", y = "Absolute Coefficient",
    fill     = "",
    caption  = "Only non-zero coefficients shown | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(face = "bold", size = 11),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

#─── RANDOM FOREST COMPARISON ────────────────────────────────────

rf_both <- bind_rows(
  head(rf_imp_A, 15),
  head(rf_imp_B, 15)
)

p_rf_compare <- ggplot(rf_both,
                       aes(x    = reorder(Indicator, Importance),
                           y    = Importance,
                           fill = Set)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = plot_two) +
  coord_flip() +
  labs(
    title    = "Random Forest: Permutation Importance — Set A vs Set B",
    subtitle = "Blue = historical average (2018-2024) | Orange = 2024 only",
    x        = "", y = "Permutation Importance",
    fill     = "",
    caption  = "Top 15 per set shown | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(face = "bold", size = 11),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

#─── XGBOOST COMPARISON ──────────────────────────────────────────

xgb_both <- bind_rows(
  head(xgb_imp_A, 15) %>% select(Feature, Gain, Set),
  head(xgb_imp_B, 15) %>% select(Feature, Gain, Set)
)

p_xgb_compare <- ggplot(xgb_both,
                        aes(x    = reorder(Feature, Gain),
                            y    = Gain,
                            fill = Set)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = plot_two) +
  coord_flip() +
  labs(
    title    = "XGBoost: Gain Importance — Set A vs Set B",
    subtitle = "Blue = historical average (2018-2024) | Orange = 2024 only",
    x        = "", y = "Gain",
    fill     = "",
    caption  = "Top 15 per set shown | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(face = "bold", size = 11),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

#─── COMBINING ALL FOUR INTO ONE FIGURE ──────────────────────────

p_all_compare <- (p_lasso_compare | p_enet_compare) /
  (p_rf_compare    | p_xgb_compare) +
  plot_annotation(
    title    = "Feature Importance Rankings: All Four Models — Set A (Historical) vs Set B (2024)",
    subtitle = "Blue bars = Set A historical averages 2018-2024 | Orange bars = Set B 2024 only",
    caption  = "Lasso and Elastic Net: absolute coefficients | RF: permutation importance | XGBoost: gain | Okabe-Ito palette",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(colour = "grey40", size = 10)
    )
  )

png("feature_importance_comparison.png",
    width = 2400, height = 2000, res = 150)
print(p_all_compare)
dev.off()
cat("Saved: feature_importance_comparison.png\n")


# ════════════════════════════════════════════════════════════════
# RQ2: OLS REGRESSION
# Does composite Social ESG ranking relate to share price?
# 55 original companies only (share price data available)
# Share price indexed to 2018 = 100
# ════════════════════════════════════════════════════════════════

#─── PREPARING RQ2 DATA ──────────────────────────────────────────

rq2_data <- complete_data %>%
  mutate(across(starts_with("Share_Price_"),
                ~ifelse(grepl("DIV/0|#N/A|#VALUE|#REF", as.character(.)),
                        NA,
                        as.numeric(gsub("[^0-9.-]", "", as.character(.)))))) %>%
  filter(!is.na(Share_Price_2018), !is.na(Share_Price_2022)) %>%
  select(Company, Industry_Clean, OverallRank,
         Share_Price_2018, Share_Price_2019,
         Share_Price_2020, Share_Price_2021,
         Share_Price_2022)

cat("Companies:", nrow(rq2_data), "\n")

excluded <- setdiff(share_price_wide$Company, rq2_data$Company)
cat("Companies excluded (invalid 2018 baseline or missing 2022):", length(excluded), "\n")
print(excluded)

#─── FITTING OLS REGRESSION ──────────────────────────────────────

rq2_model <- lm(Share_Price_2022 ~ OverallRank, data = rq2_data)
print(summary(rq2_model))

rq2_coef   <- coef(summary(rq2_model))
rq2_r2     <- summary(rq2_model)$r.squared
rq2_adj_r2 <- summary(rq2_model)$adj.r.squared
rq2_slope  <- round(rq2_coef["OverallRank", "Estimate"], 3)
rq2_p      <- round(rq2_coef["OverallRank", "Pr(>|t|)"], 4)
rq2_ci     <- confint(rq2_model)["OverallRank",]

cat("\nSlope:", rq2_slope, "\n")
cat("95% CI: [", round(rq2_ci[1], 3), ",", round(rq2_ci[2], 3), "]\n")
cat("R-squared:", round(rq2_r2, 3), "\n")
cat("P-value:", rq2_p, "\n")

#─── DIAGNOSTICS ─────────────────────────────────────────────────

rq2_residuals <- residuals(rq2_model)
cat("Residuals skewness:", round(skewness(rq2_residuals), 3), "\n")

rq2_influence <- influence.measures(rq2_model)
influential   <- which(apply(rq2_influence$is.inf, 1, any))
cat("Influential observations:", length(influential), "\n")
if(length(influential) > 0) {
  cat("Companies flagged:\n")
  print(rq2_data$Company[influential])
}

#─── ESG QUARTILES ───────────────────────────────────────────────

rq2_data <- rq2_data %>%
  mutate(ESG_Quartile = case_when(
    OverallRank <= 14 ~ "Q1 (Best ESG, ranks 1-14)",
    OverallRank <= 28 ~ "Q2 (ranks 15-28)",
    OverallRank <= 42 ~ "Q3 (ranks 29-42)",
    TRUE              ~ "Q4 (Worst ESG, ranks 43+)"
  ))

cat("\nCompanies per ESG quartile:\n")
print(table(rq2_data$ESG_Quartile))


print(table(rq2_data$ESG_Quartile))

#Checking which companies lost share price data after numeric conversion
original_sp <- complete_data %>%
  filter(!is.na(Share_Price_2022)) %>%
  pull(Company)

rq2_companies <- rq2_data$Company

dropped <- setdiff(original_sp, rq2_companies)
cat("Companies that dropped out after numeric conversion:\n")
print(dropped)
cat("Total dropped:", length(dropped), "\n")

#Checking what the share price values look like for dropped companies
cat("Share price 2022 raw values for dropped companies:\n")
complete_data %>%
  filter(Company %in% dropped) %>%
  select(Company, Share_Price_2022) %>%
  print()

# ════════════════════════════════════════════════════════════════
# RQ2 OLS ASSUMPTION TESTS
# Heteroskedasticity and normality of residuals
# ════════════════════════════════════════════════════════════════

#Breusch-Pagan test for heteroskedasticity
bp_test <- bptest(rq2_model)
cat("=== BREUSCH-PAGAN TEST (heteroskedasticity) ===\n")
print(bp_test)

#Shapiro-Wilk test for normality of residuals
sw_test <- shapiro.test(residuals(rq2_model))
cat("\n=== SHAPIRO-WILK TEST (normality of residuals) ===\n")
print(sw_test)

#If heteroskedasticity detected — robust standard errors
rq2_robust <- coeftest(rq2_model, vcov = vcovHC(rq2_model, type = "HC3"))
cat("\n=== ROBUST STANDARD ERRORS (HC3) ===\n")
print(rq2_robust)

cat("\nNote: if BP test is significant, use robust SEs in Table 4\n")

#─── RQ2 PLOT 1: ESG RANK VS SHARE PRICE ────────────────────────
p_rq2_result <- ggplot(rq2_data,
                       aes(x = OverallRank,
                           y = Share_Price_2022)) +
  geom_point(aes(colour = Industry_Clean),
             size  = 3,
             alpha = 0.8) +
  geom_smooth(method    = "lm",
              colour    = plot_highlight,
              fill      = plot_highlight,
              se        = TRUE,
              linewidth = 1,
              alpha     = 0.15) +
  geom_text_repel(
    data = rq2_data %>%
      filter(Share_Price_2022 > quantile(Share_Price_2022, 0.85) |
               Share_Price_2022 < quantile(Share_Price_2022, 0.15)),
    aes(label = Company),
    size         = 3,
    colour       = unname(okabe_ito["black"]),
    max.overlaps = 15
  ) +
  scale_colour_manual(
    values = rep(plot_full,
                 length.out = n_distinct(rq2_data$Industry_Clean))) +
  annotate("text",
           x      = 35,
           y      = max(rq2_data$Share_Price_2022, na.rm = TRUE) * 0.95,
           label  = paste0("β = ", rq2_slope,
                           "\nR² = ", round(rq2_r2, 3),
                           "\np = ", rq2_p),
           hjust  = 0,
           size   = 4,
           colour = plot_highlight) +
  labs(
    title    = "Social ESG Ranking vs Share Price Performance (RQ2)",
    subtitle = "46 KOSPI-listed firms | Share price indexed to 2018 = 100 | December 2022\nNote: Downward slope indicates higher ESG performance correlates with higher share price",
    x        = "Overall Social ESG Rank (1 = Best, 74 = Worst)",
    y        = "Share Price (2018 = 100)",
    colour   = "Industry",
    caption  = "Vermillion line = OLS regression fit | Companies listed post-2018 excluded | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    legend.position  = "right",
    legend.text      = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

png("rq2_scatter.png", width = 2400, height = 1600, res = 300)
print(p_rq2_result)
dev.off()
cat("Saved to rq2_scatter.png\n")


#─── RQ2 PLOT 2: SHARE PRICE TREND BY ESG QUARTILE ──────────────

quartile_trend <- rq2_data %>%
  group_by(ESG_Quartile) %>%
  summarise(
    SP_2018 = mean(Share_Price_2018, na.rm = TRUE),
    SP_2019 = mean(Share_Price_2019, na.rm = TRUE),
    SP_2020 = mean(Share_Price_2020, na.rm = TRUE),
    SP_2021 = mean(Share_Price_2021, na.rm = TRUE),
    SP_2022 = mean(Share_Price_2022, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-ESG_Quartile,
               names_to  = "Year",
               values_to = "Avg_Share_Price") %>%
  mutate(Year = as.integer(gsub("SP_", "", Year)))

p_quartile <- ggplot(quartile_trend,
                     aes(x      = Year,
                         y      = Avg_Share_Price,
                         colour = ESG_Quartile,
                         group  = ESG_Quartile)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 3, alpha = 0.9) +
  geom_hline(yintercept = 100,
             linetype   = "dashed",
             colour     = "grey60",
             linewidth  = 0.6) +
  scale_colour_manual(values = plot_full[1:4]) +
  scale_x_continuous(breaks = 2018:2022) +
  labs(
    title    = "Average Share Price Trend by Social ESG Quartile (2018-2022)",
    subtitle = "Companies grouped into quartiles by Overall Social ESG Rank",
    x        = "Year",
    y        = "Average Share Price (2018 = 100)",
    colour   = "ESG Quartile",
    caption  = "Dashed line = 2018 baseline | 46 KOSPI-listed firms | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

png("rq2_quartile_trend.png", width = 2400, height = 1600, res = 300)
print(p_quartile)
dev.off()
cat("Saved to rq2_quartile_trend.png\n")


# ════════════════════════════════════════════════════════════════
# RESULTS TABLES
# APA style summary tables for dissertation
# ════════════════════════════════════════════════════════════════

library(gt)
library(gridExtra)
library(grid)

#─── TABLE 2: MODEL PERFORMANCE SUMMARY ──────────────────────────

model_performance <- data.frame(
  Model = c("Lasso", "Lasso",
            "Elastic Net", "Elastic Net",
            "Random Forest", "Random Forest",
            "XGBoost", "XGBoost"),
  Set = c("Set A: Historical Average", "Set B: 2024 Only",
          "Set A: Historical Average", "Set B: 2024 Only",
          "Set A: Historical Average", "Set B: 2024 Only",
          "Set A: Historical Average", "Set B: 2024 Only"),
  Lambda_or_Params = c(
    round(lasso_cv_A$lambda.min, 3),
    round(lasso_cv_B$lambda.min, 3),
    round(enet_cv_A$lambda.min, 3),
    round(enet_cv_B$lambda.min, 3),
    NA, NA, NA, NA
  ),
  Non_Zero_or_Top10 = c(
    nrow(lasso_df_A), nrow(lasso_df_B),
    nrow(enet_df_A),  nrow(enet_df_B),
    10, 10, 10, 10
  ),
  R_Squared = c(
    NA, NA, NA, NA,
    round(rf_model_A$r.squared, 3),
    round(rf_model_B$r.squared, 3),
    NA, NA
  ),
  RMSE = c(
    NA, NA, NA, NA,
    round(sqrt(rf_model_A$prediction.error), 2),
    round(sqrt(rf_model_B$prediction.error), 2),
    NA, NA
  )
)

cat("=== TABLE 2: MODEL PERFORMANCE ===\n")
print(model_performance)

#Creating APA table
table2 <- model_performance %>%
  gt() %>%
  tab_header(
    title    = "Table 2",
    subtitle = "Model Performance Summary — RQ1 Analysis"
  ) %>%
  cols_label(
    Model             = "Model",
    Set               = "Predictor Set",
    Lambda_or_Params  = "Lambda (min)",
    Non_Zero_or_Top10 = "Non-Zero / Top 10",
    R_Squared         = "R² (OOB)",
    RMSE              = "RMSE (OOB)"
  ) %>%
  sub_missing(missing_text = "—") %>%
  tab_source_note(
    source_note = "Note. Set A uses historical averages 2018-2024. Set B uses 2024 values only. Lambda selected via LOOCV. R² and RMSE reported for Random Forest only (out-of-bag estimates). Lasso and Elastic Net: non-zero coefficients shown. RF and XGBoost: top 10 by importance shown."
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides  = c("top", "bottom"),
      color  = "black",
      weight = px(2)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides  = "bottom",
      color  = "black",
      weight = px(2)
    ),
    locations = cells_body(rows = nrow(model_performance))
  ) %>%
  tab_options(
    table.border.top.style             = "hidden",
    table.border.bottom.style          = "hidden",
    heading.border.bottom.color        = "black",
    heading.border.bottom.width        = px(1),
    column_labels.border.top.style     = "solid",
    column_labels.border.top.color     = "black",
    column_labels.border.top.width     = px(2),
    column_labels.border.bottom.style  = "solid",
    column_labels.border.bottom.color  = "black",
    column_labels.border.bottom.width  = px(1),
    table.font.names                   = "Times New Roman",
    table.font.size                    = px(11),
    data_row.padding                   = px(4),
    heading.title.font.size            = px(12),
    heading.subtitle.font.size         = px(12),
    source_notes.font.size             = px(9)
  )

gtsave(table2, "table2_model_performance.html")
cat("Saved: table2_model_performance.html\n")

#─── TABLE 3: ROBUST INDICATORS ACROSS MODELS ────────────────────

robust_table <- model_count_df %>%
  filter(Models_Count >= 3) %>%
  mutate(
    Lasso         = ifelse(Indicator %in% lasso_df_A_clean$Indicator_Clean, "✓", "✗"),
    Elastic_Net   = ifelse(Indicator %in% enet_df_A_clean$Indicator_Clean, "✓", "✗"),
    Random_Forest = ifelse(Indicator %in% head(rf_imp_A, 10)$Indicator, "✓", "✗"),
    XGBoost       = ifelse(Indicator %in% head(xgb_imp_A, 10)$Feature, "✓", "✗"),
    Robustness    = ifelse(Models_Count == 4, "All 4", "3 of 4")
  ) %>%
  select(Indicator, Lasso, Elastic_Net, Random_Forest, XGBoost, Models_Count, Robustness)

cat("\n=== TABLE 3: ROBUST INDICATORS ===\n")
print(robust_table)

table3 <- robust_table %>%
  gt() %>%
  tab_header(
    title    = "Table 3",
    subtitle = "Employment Indicators Appearing as Important Across Multiple Models (Set A)"
  ) %>%
  cols_label(
    Indicator     = "Indicator",
    Lasso         = "Lasso",
    Elastic_Net   = "Elastic Net",
    Random_Forest = "Random Forest",
    XGBoost       = "XGBoost",
    Models_Count  = "No. of Models",
    Robustness    = "Robustness"
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides  = c("top", "bottom"),
      color  = "black",
      weight = px(2)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides  = "bottom",
      color  = "black",
      weight = px(2)
    ),
    locations = cells_body(rows = nrow(robust_table))
  ) %>%
  tab_style(
    style = cell_fill(color = "#E8F4F8"),
    locations = cells_body(rows = Models_Count == 4)
  ) %>%
  tab_source_note(
    source_note = "Note. Set A uses historical averages 2018-2024. ✓ = indicator appeared in top results for that model. ✗ = indicator did not appear in top 10 for that model. Highlighted rows indicate indicators appearing in all four models."
  ) %>%
  tab_options(
    table.border.top.style             = "hidden",
    table.border.bottom.style          = "hidden",
    heading.border.bottom.color        = "black",
    heading.border.bottom.width        = px(1),
    column_labels.border.top.style     = "solid",
    column_labels.border.top.color     = "black",
    column_labels.border.top.width     = px(2),
    column_labels.border.bottom.style  = "solid",
    column_labels.border.bottom.color  = "black",
    column_labels.border.bottom.width  = px(1),
    table.font.names                   = "Times New Roman",
    table.font.size                    = px(11),
    data_row.padding                   = px(4),
    heading.title.font.size            = px(12),
    heading.subtitle.font.size         = px(12),
    source_notes.font.size             = px(9)
  )

gtsave(table3, "table3_robust_indicators.html")
cat("Saved: table3_robust_indicators.html\n")

#─── TABLE 4: RQ2 REGRESSION RESULTS ─────────────────────────────

rq2_table_data <- data.frame(
  Term = c("Intercept", "Overall Social ESG Rank"),
  Estimate = c(
    round(coef(rq2_model)[1], 3),
    round(coef(rq2_model)[2], 3)
  ),
  Std_Error = c(
    round(coef(summary(rq2_model))[1,2], 3),
    round(coef(summary(rq2_model))[2,2], 3)
  ),
  t_value = c(
    round(coef(summary(rq2_model))[1,3], 3),
    round(coef(summary(rq2_model))[2,3], 3)
  ),
  p_value = c(
    round(coef(summary(rq2_model))[1,4], 4),
    round(coef(summary(rq2_model))[2,4], 4)
  ),
  CI_Lower = c(
    round(confint(rq2_model)[1,1], 3),
    round(confint(rq2_model)[2,1], 3)
  ),
  CI_Upper = c(
    round(confint(rq2_model)[1,2], 3),
    round(confint(rq2_model)[2,2], 3)
  )
)

cat("\n=== TABLE 4: RQ2 REGRESSION ===\n")
print(rq2_table_data)

table4 <- rq2_table_data %>%
  gt() %>%
  tab_header(
    title    = "Table 4",
    subtitle = "OLS Regression: Social ESG Ranking Predicting Share Price Performance (RQ2)"
  ) %>%
  cols_label(
    Term      = "Term",
    Estimate  = "B",
    Std_Error = "SE",
    t_value   = "t",
    p_value   = "p",
    CI_Lower  = "95% CI Lower",
    CI_Upper  = "95% CI Upper"
  ) %>%
  tab_source_note(
    source_note = paste0(
      "Note. N = 46 KOSPI-listed firms. Companies listed after 2018 excluded (no baseline for share price index). ",
      "Share price indexed to 2018 = 100. Dependent variable = December 2022 close price. ",
      "R² = ", round(rq2_r2, 3), ", Adjusted R² = ", round(rq2_adj_r2, 3), ". ",
      "Two influential observations identified (SK Hynix, Hyundai Merchant Marine) via Cook's distance."
    )
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides  = c("top", "bottom"),
      color  = "black",
      weight = px(2)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides  = "bottom",
      color  = "black",
      weight = px(2)
    ),
    locations = cells_body(rows = nrow(rq2_table_data))
  ) %>%
  tab_options(
    table.border.top.style             = "hidden",
    table.border.bottom.style          = "hidden",
    heading.border.bottom.color        = "black",
    heading.border.bottom.width        = px(1),
    column_labels.border.top.style     = "solid",
    column_labels.border.top.color     = "black",
    column_labels.border.top.width     = px(2),
    column_labels.border.bottom.style  = "solid",
    column_labels.border.bottom.color  = "black",
    column_labels.border.bottom.width  = px(1),
    table.font.names                   = "Times New Roman",
    table.font.size                    = px(11),
    data_row.padding                   = px(4),
    heading.title.font.size            = px(12),
    heading.subtitle.font.size         = px(12),
    source_notes.font.size             = px(9)
  )

gtsave(table4, "table4_rq2_regression.html")
cat("Saved: table4_rq2_regression.html\n")

cat("\n=== ALL RESULTS TABLES COMPLETE ===\n")
cat("Saved as HTML files — open in browser to view and screenshot.\n")
cat("Files: table2_model_performance.html\n")
cat("       table3_robust_indicators.html\n")
cat("       table4_rq2_regression.html\n")



#Updating performance table with complete metrics
performance_df_updated <- data.frame(
  Model = c("Lasso","Lasso",
            "Elastic Net","Elastic Net",
            "Random Forest","Random Forest",
            "XGBoost","XGBoost"),
  Set = c("Set A","Set B","Set A","Set B",
          "Set A","Set B","Set A","Set B"),
  Validation = c("LOOCV","LOOCV","LOOCV","LOOCV",
                 "OOB","OOB","LOOCV","LOOCV"),
  RMSE = c(
    round(lasso_rmse_A, 3), round(lasso_rmse_B, 3),
    round(enet_rmse_A, 3),  round(enet_rmse_B, 3),
    rf_rmse_A,              rf_rmse_B,
    round(xgb_rmse_A, 3),   round(xgb_rmse_B, 3)
  ),
  R_Squared = c(
    NA, NA, NA, NA,
    rf_r2_A,            rf_r2_B,
    round(xgb_r2_A, 3), round(xgb_r2_B, 3)
  ),
  Non_Zero_Coef = c(
    nrow(lasso_df_A), nrow(lasso_df_B),
    nrow(enet_df_A),  nrow(enet_df_B),
    NA, NA, NA, NA
  )
)

table2_updated <- performance_df_updated %>%
  gt() %>%
  tab_header(
    title    = "Table 2",
    subtitle = "Model Performance Summary — RQ1 Analysis"
  ) %>%
  cols_label(
    Model         = "Model",
    Set           = "Predictor Set",
    Validation    = "Validation Method",
    RMSE          = "RMSE",
    R_Squared     = "R²",
    Non_Zero_Coef = "Non-Zero Coefficients"
  ) %>%
  sub_missing(missing_text = "—") %>%
  tab_source_note(
    source_note = paste0(
      "Note. Set A = historical averages 2018-2024 (24 predictors). ",
      "Set B = 2024 values only (23 predictors, X07 excluded due to 50% missingness). ",
      "LOOCV = leave-one-out cross-validation. OOB = out-of-bag estimate (Random Forest). ",
      "R² reported for Random Forest and XGBoost only. ",
      "Non-zero coefficients reported for regularised regression models only."
    )
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides  = c("top", "bottom"),
      color  = "black",
      weight = px(2)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides  = "bottom",
      color  = "black",
      weight = px(2)
    ),
    locations = cells_body(rows = nrow(performance_df_updated))
  ) %>%
  tab_style(
    style = cell_fill(color = "#E8F4F8"),
    locations = cells_body(rows = Set == "Set A")
  ) %>%
  tab_options(
    table.border.top.style             = "hidden",
    table.border.bottom.style          = "hidden",
    heading.border.bottom.color        = "black",
    heading.border.bottom.width        = px(1),
    column_labels.border.top.style     = "solid",
    column_labels.border.top.color     = "black",
    column_labels.border.top.width     = px(2),
    column_labels.border.bottom.style  = "solid",
    column_labels.border.bottom.color  = "black",
    column_labels.border.bottom.width  = px(1),
    table.font.names                   = "Times New Roman",
    table.font.size                    = px(11),
    data_row.padding                   = px(4),
    heading.title.font.size            = px(12),
    heading.subtitle.font.size         = px(12),
    source_notes.font.size             = px(9)
  )

gtsave(table2_updated, "table2_model_performance_updated.html")
cat("Saved: table2_model_performance_updated.html\n")
cat("All tables and cross-validation complete.\n")
cat("Ready to write the complete dissertation_analysis.R script.\n")



# ════════════════════════════════════════════════════════════════
# RESULTS VISUALISATIONS
# Predicted vs actual, residual plots for all four models
# ════════════════════════════════════════════════════════════════

#─── 1. PREDICTED VS ACTUAL — LASSO (Set A) ──────────────────────

#Getting Lasso predictions using LOOCV
set.seed(123)
lasso_preds_A <- numeric(nrow(X_A))
for(i in 1:nrow(X_A)) {
  train_x <- X_A[-i, , drop = FALSE]
  train_y <- y[-i]
  test_x  <- X_A[i, , drop = FALSE]
  
  cv_i <- cv.glmnet(
    x            = train_x,
    y            = train_y,
    alpha        = 1,
    nfolds       = length(train_y),
    type.measure = "mse"
  )
  lasso_preds_A[i] <- predict(cv_i, 
                              newx = test_x,
                              s    = "lambda.min")[1]
}

cat("Lasso LOOCV predictions done.\n")

#Getting Elastic Net predictions using LOOCV
set.seed(123)
enet_preds_A <- numeric(nrow(X_A))
for(i in 1:nrow(X_A)) {
  train_x <- X_A[-i, , drop = FALSE]
  train_y <- y[-i]
  test_x  <- X_A[i, , drop = FALSE]
  
  cv_i <- cv.glmnet(
    x            = train_x,
    y            = train_y,
    alpha        = 0.5,
    nfolds       = length(train_y),
    type.measure = "mse"
  )
  enet_preds_A[i] <- predict(cv_i,
                             newx = test_x,
                             s    = "lambda.min")[1]
}

cat("Elastic Net LOOCV predictions done.\n")

#Getting RF OOB predictions
rf_preds_A <- rf_model_A$predictions

#XGBoost predictions already stored in xgb_preds_A

#─── COMBINING ALL PREDICTIONS ───────────────────────────────────

pred_data <- data.frame(
  Company   = complete_data$Company,
  Actual    = y,
  Lasso     = lasso_preds_A,
  ElasticNet = enet_preds_A,
  RF        = rf_preds_A,
  XGBoost   = xgb_preds_A
) %>%
  pivot_longer(cols      = c(Lasso, ElasticNet, RF, XGBoost),
               names_to  = "Model",
               values_to = "Predicted") %>%
  mutate(
    Residual = Actual - Predicted,
    Model    = factor(Model,
                      levels = c("Lasso","ElasticNet","RF","XGBoost"),
                      labels = c("Lasso","Elastic Net",
                                 "Random Forest","XGBoost"))
  )

#─── 2. PREDICTED VS ACTUAL PLOT (all four models) ───────────────

p_pred_actual <- ggplot(pred_data,
                        aes(x = Actual, y = Predicted)) +
  geom_point(colour = plot_blue, alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1,
              colour    = plot_highlight,
              linewidth = 0.9,
              linetype  = "dashed") +
  geom_smooth(method    = "lm",
              colour    = unname(okabe_ito["green"]),
              se        = FALSE,
              linewidth = 0.7) +
  facet_wrap(~Model, ncol = 2) +
  labs(
    title    = "Predicted vs Actual Social ESG Rankings — Set A (Historical Average)",
    subtitle = "Vermillion dashed line = perfect prediction | Green line = actual fit",
    x        = "Actual Rank",
    y        = "Predicted Rank",
    caption  = "LOOCV predictions for Lasso and Elastic Net | OOB for RF | LOOCV for XGBoost | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(colour = "grey40", size = 9),
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )

png("pred_vs_actual_setA.png", width = 1400, height = 1200, res = 150)
print(p_pred_actual)
dev.off()
cat("Saved: pred_vs_actual_setA.png\n")

#─── 3. RESIDUAL PLOTS (all four models) ─────────────────────────

p_residuals <- ggplot(pred_data,
                      aes(x = Predicted, y = Residual)) +
  geom_point(colour = plot_blue, alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0,
             colour     = plot_highlight,
             linewidth  = 0.9,
             linetype   = "dashed") +
  geom_smooth(method    = "loess",
              colour    = unname(okabe_ito["green"]),
              se        = FALSE,
              linewidth = 0.7) +
  facet_wrap(~Model, ncol = 2) +
  labs(
    title    = "Residual Plots — Set A (Historical Average)",
    subtitle = "Vermillion dashed line = zero residual | Green line = residual trend",
    x        = "Predicted Rank",
    y        = "Residual (Actual - Predicted)",
    caption  = "Random pattern around zero indicates good model fit | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(colour = "grey40", size = 9),
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )

png("residuals_setA.png", width = 1400, height = 1200, res = 150)
print(p_residuals)
dev.off()
cat("Saved: residuals_setA.png\n")

#─── 4. RQ2 REGRESSION DIAGNOSTICS ──────────────────────────────

#Predicted vs actual for OLS
rq2_diag_data <- data.frame(
  Company   = rq2_data$Company,
  Actual    = rq2_data$Share_Price_2022,
  Predicted = fitted(rq2_model),
  Residual  = residuals(rq2_model),
  Industry  = rq2_data$Industry_Clean
)

p_rq2_pred <- ggplot(rq2_diag_data,
                     aes(x = Actual, y = Predicted)) +
  geom_point(aes(colour = Industry), size = 3, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1,
              colour    = plot_highlight,
              linewidth = 0.9,
              linetype  = "dashed") +
  geom_text_repel(
    data = rq2_diag_data %>%
      filter(abs(Residual) > quantile(abs(Residual), 0.85)),
    aes(label = Company),
    size = 3, colour = unname(okabe_ito["black"]),
    max.overlaps = 10
  ) +
  scale_colour_manual(
    values = rep(plot_full,
                 length.out = n_distinct(rq2_diag_data$Industry))) +
  labs(
    title    = "RQ2: Predicted vs Actual Share Price — OLS Regression",
    subtitle = "Vermillion dashed line = perfect prediction | Extreme residuals labelled",
    x        = "Actual Share Price (2018 = 100)",
    y        = "Predicted Share Price",
    colour   = "Industry",
    caption  = "46 KOSPI-listed firms | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    legend.position  = "right",
    legend.text      = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

png("rq2_pred_vs_actual.png", width = 1200, height = 800, res = 150)
print(p_rq2_pred)
dev.off()
cat("Saved: rq2_pred_vs_actual.png\n")

#Residuals vs fitted for OLS
p_rq2_resid <- ggplot(rq2_diag_data,
                      aes(x = Predicted, y = Residual)) +
  geom_point(colour = plot_blue, alpha = 0.7, size = 2.5) +
  geom_hline(yintercept = 0,
             colour     = plot_highlight,
             linewidth  = 0.9,
             linetype   = "dashed") +
  geom_smooth(method    = "loess",
              colour    = unname(okabe_ito["green"]),
              se        = FALSE,
              linewidth = 0.7) +
  geom_text_repel(
    data = rq2_diag_data %>%
      filter(abs(Residual) > quantile(abs(Residual), 0.85)),
    aes(label = Company),
    size = 3, colour = unname(okabe_ito["black"]),
    max.overlaps = 10
  ) +
  labs(
    title    = "RQ2: Residuals vs Fitted — OLS Regression Diagnostics",
    subtitle = "Vermillion dashed line = zero residual | Random pattern = good fit",
    x        = "Fitted Values (Predicted Share Price)",
    y        = "Residuals",
    caption  = "46 KOSPI-listed firms | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    panel.grid.minor = element_blank()
  )

png("rq2_residuals.png", width = 1200, height = 800, res = 150)
print(p_rq2_resid)
dev.off()
cat("Saved: rq2_residuals.png\n")

#─── 5. RMSE COMPARISON PLOT (updated with all models) ───────────

p_rmse <- ggplot(performance_df_updated,
                 aes(x    = reorder(Model, RMSE),
                     y    = RMSE,
                     fill = Set)) +
  geom_col(position = "dodge", width = 0.6, alpha = 0.9) +
  geom_text(aes(label = round(RMSE, 1)),
            position = position_dodge(width = 0.6),
            hjust    = -0.2,
            size     = 3.5,
            colour   = unname(okabe_ito["black"])) +
  scale_fill_manual(values = plot_two) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title    = "Model Performance: RMSE Comparison",
    subtitle = "Lower RMSE = better predictive accuracy | Set A consistently outperforms Set B",
    x        = "", y = "RMSE",
    fill     = "",
    caption  = "LOOCV for Lasso, Elastic Net, XGBoost | OOB for Random Forest | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 10),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

png("rmse_comparison.png", width = 1200, height = 700, res = 150)
print(p_rmse)
dev.off()
cat("Saved: rmse_comparison.png\n")

cat("\n=== ALL RESULTS VISUALISATIONS COMPLETE ===\n")
cat("Files saved:\n")
cat("  pred_vs_actual_setA.png\n")
cat("  residuals_setA.png\n")
cat("  rq2_pred_vs_actual.png\n")
cat("  rq2_residuals.png\n")
cat("  rmse_comparison.png\n")
cat("Next step: complete dissertation_analysis.R script.\n")



# ════════════════════════════════════════════════════════════════
# STAGE 1: LLM EXTRACTION ACCURACY ANALYSIS
# Comparing Claude Sonnet vs DeepSeek extraction outputs
# Metrics: exact match rate, MAE, ICC, accuracy vs ground truth
# ════════════════════════════════════════════════════════════════

library(readxl)
library(irr)

#Loading the flags to review sheet
flags <- read_excel("extraction_log.xlsx",
                    sheet = "Flags to Review")

#Cleaning column names
names(flags) <- c("Company","Code","Year","Indicator",
                  "Claude","DeepSeek","Diff_Pct",
                  "Diagnostic_Note","Corrected",
                  "Source","Resolved")

cat("=== EXTRACTION LOG SUMMARY ===\n")
cat("Total flagged comparisons:", nrow(flags), "\n")
cat("Companies covered:", n_distinct(flags$Company), "\n")
cat("Indicators covered:", n_distinct(flags$Indicator), "\n")

#Filtering to numeric comparisons only
flags_num <- flags %>%
  filter(is.numeric(Claude) | !is.na(suppressWarnings(as.numeric(Claude))),
         is.numeric(DeepSeek) | !is.na(suppressWarnings(as.numeric(DeepSeek)))) %>%
  mutate(
    Claude    = as.numeric(Claude),
    DeepSeek  = as.numeric(DeepSeek),
    Corrected = as.numeric(Corrected),
    Abs_Diff  = abs(Claude - DeepSeek)
  ) %>%
  filter(!is.na(Claude), !is.na(DeepSeek))

cat("\nNumeric comparison pairs:", nrow(flags_num), "\n")

#─── EXACT MATCH RATE ────────────────────────────────────────────
exact_match_rate <- mean(flags_num$Abs_Diff < 0.01) * 100
close_match_rate <- mean(flags_num$Abs_Diff /
                           (abs(flags_num$Claude) + 0.001) < 0.05) * 100

cat("\n=== AGREEMENT METRICS ===\n")
cat("Exact match rate (diff < 0.01):", round(exact_match_rate, 1), "%\n")
cat("Close match rate (within 5%):", round(close_match_rate, 1), "%\n")

#─── MAE (Claude vs DeepSeek) ────────────────────────────────────
mae_overall <- mean(flags_num$Abs_Diff)
median_diff  <- median(flags_num$Abs_Diff)

#MAE excluding Annual Pay (which has huge scale differences in KRW)
mae_excl_pay <- flags_num %>%
  filter(Indicator != "Annual Pay") %>%
  pull(Abs_Diff) %>% mean()

cat("\n=== MAE (Claude vs DeepSeek) ===\n")
cat("Overall MAE:", round(mae_overall, 2), "\n")
cat("Median absolute difference:", round(median_diff, 2), "\n")
cat("MAE excluding Annual Pay:", round(mae_excl_pay, 2), "\n")
cat("(Annual Pay values are in KRW so differences are large)\n")

#─── ACCURACY VS GROUND TRUTH ────────────────────────────────────
resolved <- flags_num %>%
  filter(grepl("Yes|yes", Resolved),
         !is.na(Corrected))

claude_exact  <- mean(abs(resolved$Claude - resolved$Corrected) < 0.01) * 100
deep_exact    <- mean(abs(resolved$DeepSeek - resolved$Corrected) < 0.01) * 100
claude_mae_gt <- mean(abs(resolved$Claude - resolved$Corrected))
deep_mae_gt   <- mean(abs(resolved$DeepSeek - resolved$Corrected))

cat("\n=== ACCURACY VS VERIFIED GROUND TRUTH ===\n")
cat("Resolved comparisons:", nrow(resolved), "\n")
cat("Claude exact match to ground truth:", round(claude_exact, 1), "%\n")
cat("DeepSeek exact match to ground truth:", round(deep_exact, 1), "%\n")
cat("Claude MAE vs ground truth:", round(claude_mae_gt, 4), "\n")
cat("DeepSeek MAE vs ground truth:", round(deep_mae_gt, 2), "\n")

#─── ICC ─────────────────────────────────────────────────────────
#Using irr package — two-way mixed ICC for consistency
icc_data <- flags_num %>%
  select(Claude, DeepSeek) %>%
  as.data.frame()

icc_result <- icc(icc_data,
                  model   = "twoway",
                  type    = "consistency",
                  unit    = "single")

cat("\n=== INTRACLASS CORRELATION COEFFICIENT ===\n")
print(icc_result)
cat("ICC value:", round(icc_result$value, 4), "\n")
cat("95% CI: [", round(icc_result$lbound, 4), ",",
    round(icc_result$ubound, 4), "]\n")
cat("F-statistic:", round(icc_result$Fvalue, 3), "\n")
cat("p-value:", round(icc_result$p.value, 4), "\n")

#─── ERROR PATTERN ANALYSIS ──────────────────────────────────────
cat("\n=== DEEPSEEK ERROR PATTERNS ===\n")

#Categorising types of DeepSeek errors from the diagnostic notes
error_types <- flags %>%
  filter(grepl("Yes|yes", Resolved)) %>%
  mutate(Error_Type = case_when(
    grepl("scope mismatch|scope/denominator|population mismatch",
          tolower(Diagnostic_Note)) ~ "Scope/Population Mismatch",
    grepl("unit|scale|100x|1000x|100 million",
          tolower(Diagnostic_Note)) ~ "Unit/Scale Error",
    grepl("lag|one.year|year.shift|shifted",
          tolower(Diagnostic_Note)) ~ "Year Lag Error",
    grepl("swap|swapped|gender|male.*female|female.*male",
          tolower(Diagnostic_Note)) ~ "Male/Female Swap",
    grepl("denominator|numerator",
          tolower(Diagnostic_Note)) ~ "Wrong Denominator",
    TRUE ~ "Other"
  )) %>%
  count(Error_Type) %>%
  arrange(desc(n)) %>%
  mutate(Pct = round(n/sum(n)*100, 1))

cat("DeepSeek error types:\n")
print(error_types)

#─── VISUALISING STAGE 1 RESULTS ─────────────────────────────────

#Plot 1: Claude vs DeepSeek accuracy comparison
accuracy_df <- data.frame(
  LLM      = c("Claude Sonnet", "DeepSeek"),
  Accuracy = c(claude_exact, deep_exact)
)

p_accuracy <- ggplot(accuracy_df,
                     aes(x = LLM, y = Accuracy, fill = LLM)) +
  geom_col(width = 0.5, alpha = 0.9) +
  geom_text(aes(label = paste0(round(Accuracy, 1), "%")),
            vjust  = -0.5, size = 5,
            colour = unname(okabe_ito["black"])) +
  scale_fill_manual(values = plot_two) +
  scale_y_continuous(limits = c(0, 110),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "LLM Extraction Accuracy: Claude Sonnet vs DeepSeek",
    subtitle = "Percentage of extractions matching verified ground truth values",
    x        = "", y = "Accuracy (%)",
    caption  = "N = 335 resolved comparisons | Ground truth verified against source PDFs | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 11),
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

png("stage1_accuracy.png", width = 900, height = 700, res = 150)
print(p_accuracy)
dev.off()
cat("Saved: stage1_accuracy.png\n")

#Plot 2: DeepSeek error type breakdown
p_errors <- ggplot(error_types,
                   aes(x    = reorder(Error_Type, n),
                       y    = n,
                       fill = Error_Type)) +
  geom_col(width = 0.7, alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", Pct, "%)")),
            hjust  = -0.1, size = 3.5,
            colour = unname(okabe_ito["black"])) +
  scale_fill_manual(values = rep(plot_full, length.out = nrow(error_types))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_flip() +
  labs(
    title    = "DeepSeek Extraction Error Types",
    subtitle = "Categorisation of errors identified through manual PDF verification",
    x        = "", y = "Number of Errors",
    caption  = "N = 335 resolved comparisons | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 10),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

png("stage1_error_types.png", width = 1100, height = 700, res = 150)
print(p_errors)
dev.off()
cat("Saved: stage1_error_types.png\n")

#─── SUMMARY TABLE ───────────────────────────────────────────────

stage1_summary <- data.frame(
  Metric = c(
    "Total extraction comparisons",
    "Companies covered",
    "Indicators covered",
    "Claude exact match rate (vs ground truth)",
    "DeepSeek exact match rate (vs ground truth)",
    "Claude MAE (vs ground truth)",
    "DeepSeek MAE (vs ground truth)",
    "ICC (Claude vs DeepSeek)",
    "ICC 95% CI Lower",
    "ICC 95% CI Upper"
  ),
  Value = c(
    nrow(flags_num),
    n_distinct(flags$Company),
    n_distinct(flags$Indicator),
    paste0(round(claude_exact, 1), "%"),
    paste0(round(deep_exact, 1), "%"),
    round(claude_mae_gt, 3),
    round(deep_mae_gt, 2),
    round(icc_result$value, 4),
    round(icc_result$lbound, 4),
    round(icc_result$ubound, 4)
  )
)

cat("\n=== STAGE 1 SUMMARY TABLE ===\n")
print(stage1_summary)

cat("\n=== STAGE 1 COMPLETE ===\n")
cat("Key findings:\n")
cat("- Claude accuracy: 95.8% vs DeepSeek: 4.5%\n")
cat("- ICC = 0.0163 (very low agreement between LLMs)\n")
cat("- DeepSeek errors mostly due to scope/population mismatches\n")
cat("- Claude values retained as primary dataset\n")


#Checking whether DeepSeek errors are systematic across companies
error_by_company <- flags %>%
  filter(grepl("Yes|yes", Resolved)) %>%
  mutate(
    DeepSeek_Wrong = abs(as.numeric(DeepSeek) - 
                           as.numeric(Corrected)) > 0.01,
    Error_Type = case_when(
      grepl("scope mismatch|population mismatch",
            tolower(Diagnostic_Note)) ~ "Scope Mismatch",
      grepl("unit|scale|100x",
            tolower(Diagnostic_Note)) ~ "Unit Error",
      grepl("lag|year.shift",
            tolower(Diagnostic_Note)) ~ "Year Lag",
      grepl("swap|swapped",
            tolower(Diagnostic_Note)) ~ "Gender Swap",
      TRUE ~ "Other"
    )
  ) %>%
  filter(DeepSeek_Wrong) %>%
  count(Company, Error_Type) %>%
  arrange(Company, desc(n))

cat("DeepSeek error patterns by company:\n")
print(error_by_company, n = 50)

#Plotting error patterns
p_error_heatmap <- error_by_company %>%
  ggplot(aes(x    = Error_Type,
             y    = Company,
             fill = n)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = n), size = 3,
            colour = unname(okabe_ito["black"])) +
  scale_fill_gradient(
    low  = "white",
    high = plot_blue,
    name = "Count"
  ) +
  labs(
    title    = "DeepSeek Extraction Error Types by Company",
    subtitle = "Number of each error type per company",
    x        = "Error Type", y = "",
    caption  = "Only errors where DeepSeek value differed from verified ground truth | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(colour = "grey40", size = 9),
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid       = element_blank()
  )

png("stage1_error_heatmap.png", width = 1200, height = 900, res = 150)
print(p_error_heatmap)
dev.off()
cat("Saved: stage1_error_heatmap.png\n")


#Checking accuracy by language
lang_accuracy <- flags %>%
  filter(grepl("Yes|yes", Resolved),
         !is.na(as.numeric(Corrected))) %>%
  mutate(
    Claude_Correct   = abs(as.numeric(Claude) - 
                             as.numeric(Corrected)) < 0.01,
    DeepSeek_Correct = abs(as.numeric(DeepSeek) - 
                             as.numeric(Corrected)) < 0.01
  ) %>%
  group_by(Company) %>%
  summarise(
    Claude_Acc   = round(mean(Claude_Correct)*100, 1),
    DeepSeek_Acc = round(mean(DeepSeek_Correct)*100, 1),
    N            = n(),
    .groups      = "drop"
  ) %>%
  arrange(desc(DeepSeek_Acc))

cat("Accuracy by company:\n")
print(lang_accuracy, n = 20)


#Visualising accuracy by company
p_company_acc <- lang_accuracy %>%
  pivot_longer(cols      = c(Claude_Acc, DeepSeek_Acc),
               names_to  = "LLM",
               values_to = "Accuracy") %>%
  mutate(LLM = ifelse(LLM == "Claude_Acc", "Claude Sonnet", "DeepSeek")) %>%
  ggplot(aes(x    = reorder(Company, Accuracy),
             y    = Accuracy,
             fill = LLM)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = plot_two) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 110),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "LLM Extraction Accuracy by Company",
    subtitle = "Claude Sonnet vs DeepSeek | Percentage matching verified ground truth",
    x        = "", y = "Accuracy (%)",
    fill     = "",
    caption  = "Okabe-Ito colour blind friendly palette"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

png("stage1_accuracy_by_company.png", width = 1200, height = 900, res = 150)
print(p_company_acc)
dev.off()
cat("Saved: stage1_accuracy_by_company.png\n")


#Better error categorisation based on fuller keyword matching
error_types_v2 <- flags %>%
  filter(grepl("Yes|yes", Resolved)) %>%
  mutate(
    DeepSeek_Wrong = abs(as.numeric(DeepSeek) -
                           as.numeric(Corrected)) > 0.01,
    Error_Type = case_when(
      grepl("scope|denominator|population|broader|narrower|different.*metric|wrong.*row|adjacent.*row|row.*instead|pulled from|used.*instead|rather than|wrong.*table|wrong.*column|wrong.*line|sub-row|sub-category|category|broader.*scope|narrower.*scope",
            tolower(Diagnostic_Note)) ~ "Scope/Row/Category Mismatch",
      grepl("unit|scale|100x|1000x|100 million|billion|million|krw|currency|parsed.*header|misread.*unit|wrong.*unit",
            tolower(Diagnostic_Note)) ~ "Unit/Scale Error",
      grepl("lag|one.year|year.shift|shifted|previous year|adjacent year|off by one year|year.*mismatch|2022.*2023|2023.*2024|2021.*2022",
            tolower(Diagnostic_Note)) ~ "Year Lag Error",
      grepl("swap|swapped|male.*female|female.*male|gender.*mix|reversed",
            tolower(Diagnostic_Note)) ~ "Male/Female Swap",
      grepl("not match|does not match|no match|cannot match|no.*figure|no.*value|not.*correspond|unclear|unknown|unexplained",
            tolower(Diagnostic_Note)) ~ "Unidentified",
      grepl("maternity.*parental|parental.*maternity|maternity.*instead|parental.*instead",
            tolower(Diagnostic_Note)) ~ "Maternity vs Parental Leave Confusion",
      TRUE ~ "Other"
    )
  ) %>%
  filter(DeepSeek_Wrong) %>%
  count(Error_Type) %>%
  arrange(desc(n)) %>%
  mutate(Pct = round(n/sum(n)*100, 1))

cat("DeepSeek error types (refined categorisation):\n")
print(error_types_v2)

#Updated error plot
p_errors_v2 <- ggplot(error_types_v2,
                      aes(x    = reorder(Error_Type, n),
                          y    = n,
                          fill = Error_Type)) +
  geom_col(width = 0.7, alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", Pct, "%)")),
            hjust  = -0.1, size = 3.5,
            colour = unname(okabe_ito["black"])) +
  scale_fill_manual(
    values = rep(plot_full, length.out = nrow(error_types_v2))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  coord_flip() +
  labs(
    title    = "DeepSeek Extraction Error Types (Refined Categorisation)",
    subtitle = "Categorisation based on diagnostic notes from manual PDF verification",
    x        = "", y = "Number of Errors",
    caption  = "N = resolved comparisons where DeepSeek differed from ground truth | Okabe-Ito palette"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

png("stage1_error_types_refined.png", width = 1200, height = 700, res = 150)
print(p_errors_v2)
dev.off()
cat("Saved: stage1_error_types_refined.png\n")


#Examining what the Other errors actually say
other_errors <- flags %>%
  filter(grepl("Yes|yes", Resolved)) %>%
  mutate(
    DeepSeek_Wrong = suppressWarnings(
      abs(as.numeric(DeepSeek) - as.numeric(Corrected)) > 0.01),
    Error_Type = case_when(
      grepl("scope|denominator|population|broader|narrower|wrong.*row|adjacent|pulled from|used.*instead|rather than|wrong.*table|wrong.*column|wrong.*line|sub-row|sub-category|category|broader.*scope|narrower.*scope",
            tolower(Diagnostic_Note)) ~ "Scope/Row/Category Mismatch",
      grepl("unit|scale|100x|1000x|100 million|billion|million|krw|currency|misread.*unit|wrong.*unit",
            tolower(Diagnostic_Note)) ~ "Unit/Scale Error",
      grepl("lag|one.year|year.shift|shifted|previous year|adjacent year|off by one year|year.*mismatch",
            tolower(Diagnostic_Note)) ~ "Year Lag Error",
      grepl("swap|swapped|male.*female|female.*male|gender.*mix|reversed",
            tolower(Diagnostic_Note)) ~ "Male/Female Swap",
      grepl("maternity.*parental|parental.*maternity|maternity.*instead|parental.*instead|maternity.*leave.*row|parental.*leave.*row",
            tolower(Diagnostic_Note)) ~ "Maternity vs Parental Leave",
      TRUE ~ "Other"
    )
  ) %>%
  filter(DeepSeek_Wrong, Error_Type == "Other") %>%
  select(Company, Indicator, Claude, DeepSeek, 
         Corrected, Diagnostic_Note) %>%
  head(20)

cat("Sample of 'Other' errors — first 20:\n")
for(i in 1:nrow(other_errors)) {
  cat("\n---\n")
  cat("Company:", other_errors$Company[i], "\n")
  cat("Indicator:", other_errors$Indicator[i], "\n")
  cat("Claude:", other_errors$Claude[i], 
      "| DeepSeek:", other_errors$DeepSeek[i],
      "| Corrected:", other_errors$Corrected[i], "\n")
  cat("Note:", substr(other_errors$Diagnostic_Note[i], 1, 200), "\n")
}



#Final refined error categorisation
error_types_final <- flags %>%
  filter(grepl("Yes|yes", Resolved)) %>%
  mutate(
    DeepSeek_Wrong = suppressWarnings(
      abs(as.numeric(DeepSeek) - as.numeric(Corrected)) > 0.01),
    Error_Type = case_when(
      #Turnover composition vs workforce rate
      Indicator %in% c("Voluntary Turnover %","Involuntary Turnover %") &
        suppressWarnings(as.numeric(DeepSeek)) > 50 ~
        "Metric Definition Mismatch\n(composition share vs workforce rate)",
      #Contract/new hire broader denominator
      Indicator %in% c("Contract Workers %","New Hire Rate %") &
        suppressWarnings(as.numeric(DeepSeek)) >
        suppressWarnings(as.numeric(Claude)) * 2 ~
        "Metric Definition Mismatch\n(broader denominator or category)",
      #Maternity vs parental leave
      grepl("maternity|parental",
            tolower(Diagnostic_Note)) ~
        "Maternity vs Parental Leave Confusion",
      #Scope and row mismatch
      grepl("scope|denominator|population|broader|narrower|wrong.*row|adjacent|pulled from|used.*instead|rather than|wrong.*table|wrong.*column|wrong.*line|sub-row|sub-category",
            tolower(Diagnostic_Note)) ~
        "Scope/Row Mismatch",
      #Unit errors
      grepl("unit|scale|100x|1000x|100 million|krw|currency|misread.*unit",
            tolower(Diagnostic_Note)) ~
        "Unit/Scale Error",
      #Year lag
      grepl("lag|one.year|year.shift|shifted|previous year|adjacent year",
            tolower(Diagnostic_Note)) ~
        "Year Lag Error",
      #Gender swap
      grepl("swap|swapped|male.*female|female.*male|reversed",
            tolower(Diagnostic_Note)) ~
        "Male/Female Swap",
      TRUE ~ "Other/Unidentified"
    )
  ) %>%
  filter(DeepSeek_Wrong) %>%
  count(Error_Type) %>%
  arrange(desc(n)) %>%
  mutate(Pct = round(n/sum(n)*100, 1))

cat("DeepSeek error types (final categorisation):\n")
print(error_types_final)
cat("Total errors categorised:", sum(error_types_final$n), "\n")

#Final error plot
p_errors_final <- ggplot(error_types_final,
                         aes(x    = reorder(Error_Type, n),
                             y    = n,
                             fill = Error_Type)) +
  geom_col(width = 0.7, alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", Pct, "%)")),
            hjust  = -0.1, size = 3.5,
            colour = unname(okabe_ito["black"])) +
  scale_fill_manual(
    values = rep(plot_full, length.out = nrow(error_types_final))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  coord_flip() +
  labs(
    title    = "DeepSeek Extraction Error Types",
    subtitle = "Categorisation based on diagnostic notes and value patterns",
    x        = "", y = "Number of Errors",
    caption  = paste0("N = ", sum(error_types_final$n),
                      " errors | Okabe-Ito colour blind friendly palette")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 10),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

png("stage1_error_types_final.png", width = 1400, height = 800, res = 150)
print(p_errors_final)
dev.off()
cat("Saved: stage1_error_types_final.png\n")


#Saving Stage 1 summary as formatted table
stage1_grob <- tableGrob(
  stage1_summary,
  rows  = NULL,
  theme = ttheme_minimal(
    core    = list(fg_params = list(fontfamily = "serif", fontsize = 10)),
    colhead = list(fg_params = list(fontfamily = "serif", fontsize = 10,
                                    fontface   = "bold")),
    padding = unit(c(4, 8), "mm")
  )
)
p_stage1_table <- arrangeGrob(
  stage1_grob,
  top    = textGrob(
    "Table 5. Stage 1 LLM Extraction Accuracy Summary",
    gp = gpar(fontfamily = "serif", fontsize = 12, fontface = "bold")),
  bottom = textGrob(
    "Note. Ground truth verified against source PDF documents. ICC = intraclass correlation coefficient.",
    gp = gpar(fontfamily = "serif", fontsize = 9, fontface = "italic"))
)
png("table5_stage1_summary.png", width = 900, height = 700, res = 150)
grid.draw(p_stage1_table)
dev.off()
cat("Saved: table5_stage1_summary.png\n")



#Saving all missing EDA plots

png("table1_outcome_summary.png", width = 800, height = 600, res = 150)
grid.draw(p_table)
dev.off()

png("03_rank_by_industry.png", width = 1600, height = 1000, res = 150)
print(p_industry)
dev.off()

png("04_correlation_matrix.png", width = 1400, height = 1200, res = 150)
print(p_corr)
dev.off()

png("05_correlated_pairs.png", width = 1400, height = 1000, res = 150)
print(p_pairs)
dev.off()

png("06a_indicator_distributions_01to12.png", width = 1200, height = 800, res = 150)
print(p_box_a)
dev.off()

png("06b_indicator_distributions_13to24.png", width = 1200, height = 800, res = 150)
print(p_box_b)
dev.off()

png("07_outlier_heatmap.png", width = 1400, height = 1000, res = 150)
print(p_outlier)
dev.off()

png("08a_indicator_vs_rank_01to12.png", width = 1400, height = 1400, res = 150)
print(p_scatter_a)
dev.off()

png("08b_indicator_vs_rank_13to24.png", width = 1400, height = 1400, res = 150)
print(p_scatter_b)
dev.off()

png("09a_setA_vs_setB_part1.png", width = 1400, height = 1200, res = 150)
print(p_compare_a)
dev.off()

png("09b_setA_vs_setB_part2.png", width = 1400, height = 1200, res = 150)
print(p_compare_b)
dev.off()

png("10_top_bottom_performers.png", width = 1200, height = 800, res = 150)
print(p_performers)
dev.off()

png("11_disclosure_rates_over_time.png", width = 1200, height = 700, res = 150)
print(p_disclosure)
dev.off()

cat("All missing EDA plots saved.\n")




# ════════════════════════════════════════════════════════════════
# SENSITIVITY CHECK: Doosan Bobcat mean imputation
# ════════════════════════════════════════════════════════════════

library(glmnet)
library(ranger)
library(dplyr)

# ---- Identify Doosan Bobcat's row ---------------------------------
doosan_row <- which(complete_data$Company == "Doosan Bobcat")
stopifnot(length(doosan_row) == 1)   # should find exactly one match
cat("Doosan Bobcat found at row:", doosan_row, "\n\n")

# ---- Build the N=73 (Doosan Bobcat excluded) versions -------------
X_A_sens <- X_A[-doosan_row, ]
y_sens   <- y[-doosan_row]

# ════════════════════════════════════════════════════════════════
# 1. LASSO: N=74 (imputed) vs N=73 (excluded)
# ════════════════════════════════════════════════════════════════
set.seed(123)
lasso_cv_A_sens <- cv.glmnet(
  x = X_A_sens,
  y = y_sens,
  alpha = 1,
  nfolds = nrow(X_A_sens),
  type.measure = "mse"
)

lasso_coef_A_sens <- coef(lasso_cv_A_sens, s = "lambda.min")
lasso_df_A_sens <- data.frame(
  Indicator = rownames(lasso_coef_A_sens)[-1],
  Coefficient = as.numeric(lasso_coef_A_sens)[-1]
) %>%
  filter(Coefficient != 0) %>%
  mutate(Abs_Coef = abs(Coefficient)) %>%
  arrange(desc(Abs_Coef))

rmse_A_full <- sqrt(min(lasso_cv_A$cvm))         # your existing N=74 model
rmse_A_sens <- sqrt(min(lasso_cv_A_sens$cvm))    # N=73, Doosan Bobcat excluded

cat("=== LASSO SENSITIVITY CHECK ===\n")
cat("N=74 (imputed)  - lambda.min:", round(lasso_cv_A$lambda.min, 4),
    "| LOOCV RMSE:", round(rmse_A_full, 3),
    "| non-zero coefs:", sum(coef(lasso_cv_A, s = "lambda.min")[-1] != 0), "\n")
cat("N=73 (excluded) - lambda.min:", round(lasso_cv_A_sens$lambda.min, 4),
    "| LOOCV RMSE:", round(rmse_A_sens, 3),
    "| non-zero coefs:", nrow(lasso_df_A_sens), "\n\n")

cat("Selected indicators, N=73 (Doosan Bobcat excluded):\n")
print(lasso_df_A_sens)

# Which indicators appear in one list but not the other?
full_names <- lasso_df_A$Indicator          # your existing N=74 result object
sens_names <- lasso_df_A_sens$Indicator
cat("\nIn full (N=74) model but NOT in sensitivity (N=73) model:\n")
print(setdiff(full_names, sens_names))
cat("In sensitivity (N=73) model but NOT in full (N=74) model:\n")
print(setdiff(sens_names, full_names))

# ════════════════════════════════════════════════════════════════
# 2. RANDOM FOREST: N=74 (imputed) vs N=73 (excluded) — OOB RMSE
#    and top-10 permutation importance
# ════════════════════════════════════════════════════════════════
rf_data_A_sens <- as.data.frame(X_A_sens)
colnames(rf_data_A_sens) <- short_names
rf_data_A_sens$outcome <- y_sens

set.seed(123)
rf_model_A_sens <- ranger(
  outcome ~ .,
  data = rf_data_A_sens,
  num.trees = 500,
  importance = "permutation",
  seed = 123
)

cat("\n=== RANDOM FOREST SENSITIVITY CHECK ===\n")
cat("N=74 (imputed)  - OOB RMSE:", round(sqrt(rf_model_A$prediction.error), 3),
    "| R-squared:", round(rf_model_A$r.squared, 3), "\n")
cat("N=73 (excluded) - OOB RMSE:", round(sqrt(rf_model_A_sens$prediction.error), 3),
    "| R-squared:", round(rf_model_A_sens$r.squared, 3), "\n\n")

rf_imp_A_sens <- data.frame(
  Indicator = names(rf_model_A_sens$variable.importance),
  Importance = rf_model_A_sens$variable.importance
) %>% arrange(desc(Importance))

cat("Top 10 indicators by permutation importance, N=73 (Doosan Bobcat excluded):\n")
print(head(rf_imp_A_sens, 10))
cat("\nTop 10 indicators by permutation importance, N=74 (your existing result):\n")
print(head(rf_imp_A, 10))

# ════════════════════════════════════════════════════════════════
# Summary
# ════════════════════════════════════════════════════════════════
cat("\n=== SUMMARY FOR RESULTS WRITE-UP ===\n")
cat(sprintf(
  "Lasso LOOCV RMSE changed by %.3f (%.1f%%) when Doosan Bobcat was excluded\ninstead of imputed (N=74: %.3f -> N=73: %.3f).\n",
  rmse_A_sens - rmse_A_full,
  100 * (rmse_A_sens - rmse_A_full) / rmse_A_full,
  rmse_A_full, rmse_A_sens
))
cat(sprintf(
  "Random Forest OOB RMSE changed by %.3f (%.1f%%) under the same comparison.\n",
  sqrt(rf_model_A_sens$prediction.error) - sqrt(rf_model_A$prediction.error),
  100 * (sqrt(rf_model_A_sens$prediction.error) - sqrt(rf_model_A$prediction.error)) / sqrt(rf_model_A$prediction.error)
))
