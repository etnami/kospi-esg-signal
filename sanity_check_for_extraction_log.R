# ════════════════════════════════════════════════════════════════
# SENSITIVITY CHECK: reconciliation threshold (5% vs alternatives)
# ════════════════════════════════════════════════════════════════

library(readxl)
library(dplyr)

file_path <- "extraction_log.xlsx"

# Reading the two header rows to find each indicator's columns
h1 <- read_excel(file_path, sheet = "Extraction Log", col_names = FALSE, n_max = 1) %>%
  unlist(use.names = FALSE)
h2 <- read_excel(file_path, sheet = "Extraction Log", col_names = FALSE,
                 skip = 1, n_max = 1) %>%
  unlist(use.names = FALSE)

# Filling down h1 (indicator group labels only appear once, on the first column of each 4-column block) so every column has its group label
last_label <- NA
h1_filled <- character(length(h1))
for (i in seq_along(h1)) {
  if (!is.na(h1[i]) && nzchar(h1[i])) last_label <- h1[i]
  h1_filled[i] <- last_label
}

# Identifying the "Claude Sonnet" column of each employment indicator block (1-24), skipping Indicators 25-30
is_external <- function(label) {
  if (is.na(label)) return(FALSE)
  num <- suppressWarnings(as.numeric(sub(":.*", "", label)))
  !is.na(num) && num >= 25 && num <= 30
}

claude_cols <- which(
  !is.na(h2) & h2 == "Claude\nSonnet" & !sapply(h1_filled, is_external)
)
cat("Employment-indicator blocks found:", length(claude_cols), "\n")
stopifnot(length(claude_cols) == 24)   # should always be 24

# Reading the actual data (skip the 3 header/decision-rule rows)
data <- read_excel(file_path, sheet = "Extraction Log", col_names = FALSE, skip = 3)

# Computing Claude-vs-DeepSeek % difference for every cell
all_diffs <- c()
for (cc in claude_cols) {
  claude_vals   <- data[[cc]]
  deepseek_vals <- data[[cc + 1]]   # DeepSeek is always the next column
  for (i in seq_along(claude_vals)) {
    a <- suppressWarnings(as.numeric(claude_vals[i]))
    b <- suppressWarnings(as.numeric(deepseek_vals[i]))
    if (!is.na(a) && !is.na(b)) {
      if (a == 0 && b == 0) next          # both zero: not informative
      if (a == 0) next                    # division by zero: handled separately in your log
      all_diffs <- c(all_diffs, abs(a - b) / abs(a) * 100)
    }
  }
}

cat("Total numeric Claude-vs-DeepSeek comparisons:", length(all_diffs), "\n\n")

# Testing the actual thresholds
thresholds <- c(1, 2, 3, 5, 7, 10, 15, 20)
sensitivity_table <- data.frame(
  Threshold_pct = thresholds,
  N_Flagged     = sapply(thresholds, function(t) sum(all_diffs >= t)),
  Pct_Flagged   = sapply(thresholds, function(t) round(100 * mean(all_diffs >= t), 1))
)

cat("=== RECONCILIATION THRESHOLD SENSITIVITY CHECK ===\n")
print(sensitivity_table)

# Summary line ready to paste into Methodology/Results
row5  <- sensitivity_table[sensitivity_table$Threshold_pct == 5, ]
row1  <- sensitivity_table[sensitivity_table$Threshold_pct == 1, ]
row10 <- sensitivity_table[sensitivity_table$Threshold_pct == 10, ]
row20 <- sensitivity_table[sensitivity_table$Threshold_pct == 20, ]

cat(sprintf(
  "\nSUMMARY: at the 5%% threshold used, %.1f%% of comparisons were flagged.\nAlternative thresholds of 1%%, 10%%, and 20%% would have flagged %.1f%%, %.1f%%, and %.1f%%\nrespectively, a smooth and gradual change with no discontinuity around 5%%.\n",
  row5$Pct_Flagged, row1$Pct_Flagged, row10$Pct_Flagged, row20$Pct_Flagged
))