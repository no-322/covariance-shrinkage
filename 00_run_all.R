# 00_run_all.R
# Master script — sources all files and runs the full analysis
# Run this from the project root directory

# ============================================================
# SETUP
# ============================================================
library(rugarch)
library(PeerPerformance)

source("01_load_data.R")
source("02_backtest.R")
source("03_estimators.R")
source("04_tests.R")
source("05_results.R")

# ============================================================
# PREPARE DATA
# ============================================================

# --- 47 Industries ---
R_indus <- as.matrix(filtered_indus)

# --- 74 Stocks (needs data/stock_returns_clean.csv, see README) ---
source("01b_load_stocks.R")
R_stocks <- as.matrix(filtered_stocks)


# ============================================================
# RUN BACKTESTS — 47 Industries
# ============================================================
cat("Running backtests on 47 Industries...\n")

sample_cov <- function(R) cov(R)

cat("  Sample covariance...\n")
bt_sample_indus <- run_backtest(R_indus, ym, mkt, estimator_fn = sample_cov)

cat("  LW Constant Correlation...\n")
bt_lwcc_indus   <- run_backtest(R_indus, ym, mkt, estimator_fn = lw_constant_corr)

cat("  LW Index Model...\n")
bt_lwidx_indus  <- run_backtest(R_indus, ym, mkt, estimator_fn = lw_index_model)

cat("  GARCH (this will take a while)...\n")
bt_garch_indus  <- run_backtest(R_indus, ym, mkt, estimator_fn = garch_cov)

cat("  Equal Weight...\n")
bt_eqw_indus    <- run_equal_weight(R_indus, ym)

cat("Done.\n\n")


# ============================================================
# RUN BACKTESTS — 74 Stocks
# ============================================================
cat("Running backtests on 74 Stocks...\n")

bt_sample_stocks <- run_backtest(R_stocks, ym_stocks, mkt_stocks,
                                  estimator_fn = sample_cov)
bt_lwcc_stocks   <- run_backtest(R_stocks, ym_stocks, mkt_stocks,
                                  estimator_fn = lw_constant_corr)
bt_lwidx_stocks  <- run_backtest(R_stocks, ym_stocks, mkt_stocks,
                                  estimator_fn = lw_index_model)
bt_garch_stocks  <- run_backtest(R_stocks, ym_stocks, mkt_stocks,
                                  estimator_fn = garch_cov)
bt_eqw_stocks    <- run_equal_weight(R_stocks, ym_stocks)

cat("Done.\n\n")


# ============================================================
# SUMMARY TABLES
# ============================================================

# --- GMVP comparison ---
gmvp_list <- list(
  "Sample"   = bt_sample_indus$gmvp,
  "LW-CC"    = bt_lwcc_indus$gmvp,
  "LW-Index" = bt_lwidx_indus$gmvp,
  "GARCH"    = bt_garch_indus$gmvp
)

cat("=== GMVP Summary Table (47 Industries) ===\n")
gmvp_table <- build_summary_table(gmvp_list, bt_eqw_indus)
print(gmvp_table)

# --- Max Beta/Vol comparison ---
maxbv_list <- list(
  "Sample"   = bt_sample_indus$maxbv,
  "LW-CC"    = bt_lwcc_indus$maxbv,
  "LW-Index" = bt_lwidx_indus$maxbv,
  "GARCH"    = bt_garch_indus$maxbv
)

cat("\n=== Max Beta/Vol Summary Table (47 Industries) ===\n")
maxbv_table <- build_summary_table(maxbv_list)
print(maxbv_table)


# ============================================================
# STATISTICAL TESTS
# ============================================================
cat("\n=== Statistical Tests (GMVP, 47 Industries) ===\n")
run_all_tests(gmvp_list)


# ============================================================
# CRISIS PERIOD ANALYSIS
# ============================================================
cat("\n=== Crisis Period Analysis ===\n")

# Define crisis windows as YYYYMM ranges
crisis_windows <- list(
  "Dot-Com (2001-2002)"   = c(200101, 200212),
  "GFC (2008-2009)"       = c(200809, 200903),
  "COVID (2020)"          = c(202002, 202003)
)

crisis_results <- run_crisis_analysis(gmvp_list, bt_eqw_indus, crisis_windows)
print(crisis_results)


# ============================================================
# PLOTS
# ============================================================
cat("\nGenerating plots...\n")

# Cumulative return plot — GMVP
plot_cumulative_returns(gmvp_list, bt_eqw_indus,
                        title = "Cumulative Returns: GMVP (47 Industries)")

# Bar chart of key metrics
plot_metric_comparison(gmvp_table,
                       title = "GMVP Performance Comparison (47 Industries)")

# Realized vol over time
plot_rolling_vol(gmvp_list, bt_eqw_indus,
                 title = "Rolling 12-Month Realized Vol (47 Industries)")

cat("All plots saved.\n")
cat("\n=== Analysis Complete ===\n")
