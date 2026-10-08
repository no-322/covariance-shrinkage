# 01b_load_stocks.R
# Loads cleaned stock return data and aligns with RF and market from French file
# Assumes 01_load_data.R has already been sourced (for rf, mkt, ym)

stocks_raw <- read.csv("data/stock_returns_clean.csv")

# Filter to same date range as industries (should already be 2001-2025)
stocks_raw <- stocks_raw[stocks_raw$date > 20001231 & stocks_raw$date < 20260101, ]

# Align dates with the French data (inner join on date)
# The French file has the same trading days, but there might be minor mismatches
common_dates <- intersect(indus$X, stocks_raw$date)
stocks_raw   <- stocks_raw[stocks_raw$date %in% common_dates, ]
indus_aligned <- indus[indus$X %in% common_dates, ]

# RF from the French file, aligned to the same dates
rf_stocks <- indus_aligned$RF

# Market return from French file
mkt_stocks <- indus_aligned$RmRF / 100

# Year-month for rebalancing
ym_stocks <- stocks_raw$date %/% 100

# Extract stock columns (drop date), subtract RF, convert to decimal
filtered_stocks <- stocks_raw[, -1]                       # drop date column
filtered_stocks <- (filtered_stocks - rf_stocks) / 100    # excess returns in decimal

# Sanity checks
cat("Stock data loaded:\n")
cat("  Dimensions:", dim(filtered_stocks)[1], "x", dim(filtered_stocks)[2], "\n")
cat("  Date range:", min(common_dates), "to", max(common_dates), "\n")
cat("  Total NAs:", sum(is.na(filtered_stocks)), "\n")
cat("  Ann. mean:", round(mean(as.matrix(filtered_stocks)) * 252, 4), "\n")
cat("  Ann. vol:", round(sd(as.matrix(filtered_stocks)) * sqrt(252), 4), "\n")
