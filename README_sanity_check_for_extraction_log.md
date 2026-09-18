# sanity_check_for_extraction_log.R

*Sensitivity Check: Reconciliation Threshold (5% vs Alternatives)*

## Overview
This is a short, self-contained R script that tests whether the 5%
reconciliation threshold used to flag Claude Sonnet vs DeepSeek
extraction disagreements (in the Stage 1 LLM extraction accuracy
analysis) was a reasonable choice, or whether the results would have
looked very different under a stricter or looser threshold. It reads
the raw two-row-header extraction log directly (not the pre-filtered
"Flags to Review" sheet used elsewhere), computes the percentage
difference between Claude and DeepSeek for every numeric employment
indicator cell, and reports what percentage of comparisons would be
flagged at eight different candidate thresholds.



## Requirements
R version 4.5.1 or later (consistent with the main analysis script).

Required packages (2, confirmed directly from this script's
library() calls):

```
readxl, dplyr
```

Install with:

```r
install.packages(c("readxl", "dplyr"))
```

Package roles:
  - readxl: reading the "Extraction Log" sheet of the .xlsx file,
    including reading individual header rows with col_names = FALSE
  - dplyr: piping (%>%) into unlist() when reading the header rows



## Data
This script expects the following input file to be present in the
working directory:

  `extraction_log.xlsx` - the same workbook used by the Stage 1 LLM
  extraction accuracy analysis in final_R_code.R, but this script
  reads a different sheet: "Extraction Log" (the raw, wide-format
  sheet with one 4-column block per indicator), not "Flags to
  Review". The expected layout is:
    - Row 1: indicator group labels (e.g. "1: <indicator name>"),
      present only in the first column of each 4-column block and
      blank/merged-looking elsewhere
    - Row 2: sub-column labels within each block, including a
      column literally labelled "Claude\nSonnet" (i.e. "Claude",
      newline, "Sonnet") followed immediately by the DeepSeek column
    - Row 3: a decision-rule row, skipped along with rows 1-2 when
      the actual data is read
    - Row 4 onward: one row per company, with numeric values in the
      Claude and DeepSeek columns

  The script assumes indicator group labels start with a number
  followed by a colon (e.g. "25: ..."); it uses that leading number
  to identify and exclude Indicators 25-30 (the external ratings),
  keeping only the 24 employment indicators. If exactly 24 "Claude
  Sonnet" columns aren't found among the non-external blocks, the
  script stops with an error (stopifnot(length(claude_cols) == 24)) -
  this is a deliberate check that the sheet layout hasn't changed.

Set your working directory to the folder containing this file before
running the script, since it is read via the bare relative filename
`extraction_log.xlsx` (set near the top of the script, in
file_path).



## Script structure (in run order)

1. LOADING LIBRARIES: readxl, dplyr

2. READING THE TWO HEADER ROWS: reads row 1 (indicator group
   labels) and row 2 (sub-column labels, including "Claude\nSonnet")
   of the "Extraction Log" sheet as plain character vectors, without
   treating either as column names

3. FILLING DOWN THE GROUP LABELS: indicator group labels appear
   only once per 4-column block, so this step carries each label
   forward across the block until the next label appears, giving
   every column an associated indicator group

4. IDENTIFYING THE CLAUDE SONNET COLUMNS: for each indicator block,
   finds the column whose row-2 label is exactly "Claude\nSonnet"
   and whose group label's leading number is NOT in the 25-30
   (external ratings) range, leaving the 24 employment-indicator
   Claude columns; DeepSeek is assumed to always be the very next
   column. Stops execution if this doesn't come to exactly 24
   columns.

5. READING THE ACTUAL DATA: re-reads the sheet from row 4 onward
   (skip = 3), i.e. skipping the two header rows and the
   decision-rule row, with no column names assigned

6. COMPUTING CLAUDE-VS-DEEPSEEK % DIFFERENCES: for every
   Claude/DeepSeek column pair and every company row: converts both
   values to numeric, and computes abs(Claude - DeepSeek) /
   abs(Claude) * 100 wherever both values are numeric. Cases where
   both values are zero are excluded as uninformative, and cases
   where only the Claude value is zero are excluded as
   division-by-zero (the script's comment notes these are handled
   separately in the extraction log itself). All qualifying
   percentage differences are pooled into a single vector
   (all_diffs), across all 24 indicators and all companies together.

7. TESTING THE CANDIDATE THRESHOLDS: for eight threshold values
   (1%, 2%, 3%, 5%, 7%, 10%, 15%, 20%), counts and calculates the
   percentage of comparisons in all_diffs that would be flagged
   (i.e. difference >= threshold) at each one, and prints the
   resulting table (sensitivity_table) to the console.

8. SUMMARY LINE: pulls the flagged percentages at the 1%, 5%, 10%,
   and 20% thresholds specifically and prints a ready-to-paste
   sentence for the Methodology/Results write-up, reporting the
   percentage flagged at the 5% threshold actually used alongside
   the three alternatives, and asserting (as written) that the
   change across thresholds is smooth and gradual with no
   discontinuity around 5%. Note: this "smooth, no discontinuity"
   description is hard-coded into the cat() output text itself,
   rather than derived from a test in the script - if you rerun this
   with different or updated data, check the printed table and
   confirm the description still holds before pasting the summary
   sentence as-is.



## Outputs produced
This script produces no files. All output is printed to the R
console:
  - "Employment-indicator blocks found: <n>" and a stopifnot() check
    that n == 24
  - "Total numeric Claude-vs-DeepSeek comparisons: <n>"
  - The sensitivity_table data frame (Threshold_pct, N_Flagged,
    Pct_Flagged for each of the 8 thresholds), printed under the
    header "=== RECONCILIATION THRESHOLD SENSITIVITY CHECK ==="
  - A final summary sentence comparing the 5% threshold's flag rate
    against the 1%, 10%, and 20% thresholds

If you want to keep a permanent record of the results, capture the
console output manually (e.g. sink()) or save sensitivity_table
yourself (e.g. write.csv(sensitivity_table, "reconciliation_
threshold_sensitivity.csv", row.names = FALSE)) - neither is done by
the script as written.



## Notes
- This script was developed and run in R 4.5.1, consistent with
  final_R_code.R. Package versions were not pinned.
- No random seed is set or needed - the script is entirely
  deterministic (it reads fixed data and computes fixed summary
  statistics; no resampling, simulation, or model fitting is
  involved).
- Runtime: not stated in the script; given its size (two targeted
  reads of the header rows plus one full read of the data, followed
  by vectorised/looped arithmetic over the comparisons), it should
  run in well under a minute on typical hardware.
- This script is independent of final_R_code.R - it does not source
  or depend on any objects created there, and can be run on its own
  provided `extraction_log.xlsx` is present.

