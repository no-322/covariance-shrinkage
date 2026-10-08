# 06_generate_all_outputs.R
# Run AFTER all backtests are complete and in memory
# Generates all tables, plots, and tests for both universes

dir.create("output", showWarnings = FALSE)

source("05_results.R")
source("04_tests.R")

# ============================================================
# 47 INDUSTRIES
# ============================================================

gmvp_indus <- list(
  "Sample"   = bt_sample_indus$gmvp,
  "LW-CC"    = bt_lwcc_indus$gmvp,
  "LW-Index" = bt_lwidx_indus$gmvp,
  "GARCH"    = bt_garch_indus$gmvp
)

maxbv_indus <- list(
  "Sample"   = bt_sample_indus$maxbv,
  "LW-CC"    = bt_lwcc_indus$maxbv,
  "LW-Index" = bt_lwidx_indus$maxbv,
  "GARCH"    = bt_garch_indus$maxbv
)

cat("=== 47 Industries: GMVP Summary ===\n")
print(build_summary_table(gmvp_indus, bt_eqw_indus))

cat("\n=== 47 Industries: Max Beta/Vol Summary ===\n")
print(build_summary_table(maxbv_indus, bt_eqw_indus))

cat("\n=== 47 Industries: GMVP Tests ===\n")
run_all_tests(gmvp_indus)

cat("\n=== 47 Industries: Crisis Analysis (GMVP) ===\n")
crisis_windows <- list(
  "Dot-Com (2001-2002)" = c(200101, 200212),
  "GFC (2008-2009)"     = c(200809, 200903),
  "COVID (2020)"        = c(202002, 202003)
)
print_crisis_table(run_crisis_analysis(gmvp_indus, bt_eqw_indus, crisis_windows))

cat("\n=== 47 Industries: Crisis Analysis (Max Beta/Vol) ===\n")
print_crisis_table(run_crisis_analysis(maxbv_indus, bt_eqw_indus, crisis_windows))

# Plots — 47 Industries
plot_rolling_return(gmvp_indus, bt_eqw_indus,
                    title = "Rolling 12-Month Return: GMVP (47 Industries)")
file.rename("output/rolling_return.png", "output/indus_gmvp_rolling_return.png")

plot_rolling_vol(gmvp_indus, bt_eqw_indus,
                 title = "Rolling 12-Month Vol: GMVP (47 Industries)")
file.rename("output/rolling_vol.png", "output/indus_gmvp_rolling_vol.png")

plot_metric_comparison(build_summary_table(gmvp_indus, bt_eqw_indus),
                       title = "GMVP Performance (47 Industries)")
file.rename("output/metric_comparison.png", "output/indus_gmvp_metrics.png")

plot_metric_comparison(build_summary_table(maxbv_indus, bt_eqw_indus),
                       title = "Max Beta/Vol Performance (47 Industries)")
file.rename("output/metric_comparison.png", "output/indus_maxbv_metrics.png")


# ============================================================
# 74 STOCKS
# ============================================================

gmvp_stocks <- list(
  "Sample"   = bt_sample_stocks$gmvp,
  "LW-CC"    = bt_lwcc_stocks$gmvp,
  "LW-Index" = bt_lwidx_stocks$gmvp,
  "GARCH"    = bt_garch_stocks$gmvp
)

maxbv_stocks <- list(
  "Sample"   = bt_sample_stocks$maxbv,
  "LW-CC"    = bt_lwcc_stocks$maxbv,
  "LW-Index" = bt_lwidx_stocks$maxbv,
  "GARCH"    = bt_garch_stocks$maxbv
)

cat("\n=== 74 Stocks: GMVP Summary ===\n")
print(build_summary_table(gmvp_stocks, bt_eqw_stocks))

cat("\n=== 74 Stocks: Max Beta/Vol Summary ===\n")
print(build_summary_table(maxbv_stocks, bt_eqw_stocks))

cat("\n=== 74 Stocks: GMVP Tests ===\n")
run_all_tests(gmvp_stocks)

cat("\n=== 74 Stocks: Crisis Analysis (GMVP) ===\n")
print_crisis_table(run_crisis_analysis(gmvp_stocks, bt_eqw_stocks, crisis_windows))

cat("\n=== 74 Stocks: Crisis Analysis (Max Beta/Vol) ===\n")
print_crisis_table(run_crisis_analysis(maxbv_stocks, bt_eqw_stocks, crisis_windows))

# Plots — 74 Stocks
plot_rolling_return(gmvp_stocks, bt_eqw_stocks,
                    title = "Rolling 12-Month Return: GMVP (74 Stocks)")
file.rename("output/rolling_return.png", "output/stocks_gmvp_rolling_return.png")

plot_rolling_vol(gmvp_stocks, bt_eqw_stocks,
                 title = "Rolling 12-Month Vol: GMVP (74 Stocks)")
file.rename("output/rolling_vol.png", "output/stocks_gmvp_rolling_vol.png")

plot_metric_comparison(build_summary_table(gmvp_stocks, bt_eqw_stocks),
                       title = "GMVP Performance (74 Stocks)")
file.rename("output/metric_comparison.png", "output/stocks_gmvp_metrics.png")

plot_metric_comparison(build_summary_table(maxbv_stocks, bt_eqw_stocks),
                       title = "Max Beta/Vol Performance (74 Stocks)")
file.rename("output/metric_comparison.png", "output/stocks_maxbv_metrics.png")

cat("\n=== All outputs generated. Check output/ folder. ===\n")