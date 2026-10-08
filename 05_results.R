# 05_results.R
# Tables, plots, and crisis-period analysis


# ============================================================
# SUMMARY TABLE BUILDER
# ============================================================
# Takes a named list of backtest results + optional equal-weight result
# Returns a data frame with one row per method

build_summary_table <- function(methods_list, eqw_bt = NULL) {
  
  rows <- list()
  
  for (name in names(methods_list)) {
    s <- summarize_backtest(methods_list[[name]])
    rows[[name]] <- data.frame(
      Method       = name,
      Ann_Mean     = round(s$ann_mean, 4),
      Ann_Vol      = round(s$ann_vol, 4),
      Sharpe       = round(s$sharpe, 3),
      Pred_Vol     = round(s$pred_ann_vol, 4),
      Pred_OOS_Gap = round(s$pred_oos_gap, 4),
      Turnover     = round(s$turnover, 2),
      Long_Leg     = round(s$long_leg, 2),
      Max_Weight   = round(s$max_weight, 3),
      Eff_N        = round(s$eff_n, 1),
      stringsAsFactors = FALSE
    )
  }
  
  # Add equal-weight if provided
  if (!is.null(eqw_bt)) {
    s <- summarize_backtest(eqw_bt)
    rows[["1/N"]] <- data.frame(
      Method       = "1/N",
      Ann_Mean     = round(s$ann_mean, 4),
      Ann_Vol      = round(s$ann_vol, 4),
      Sharpe       = round(s$sharpe, 3),
      Pred_Vol     = NA,
      Pred_OOS_Gap = NA,
      Turnover     = round(s$turnover, 2),
      Long_Leg     = round(s$long_leg, 2),
      Max_Weight   = round(s$max_weight, 3),
      Eff_N        = round(s$eff_n, 1),
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, rows)
}


# ============================================================
# CRISIS PERIOD ANALYSIS
# ============================================================
# Computes realized vol and mean for each method during crisis windows
# crisis_windows: named list of c(start_yyyymm, end_yyyymm)

run_crisis_analysis <- function(methods_list, eqw_bt = NULL, crisis_windows) {
  
  all_results <- list()
  
  for (crisis_name in names(crisis_windows)) {
    window <- crisis_windows[[crisis_name]]
    start_ym <- window[1]
    end_ym   <- window[2]
    
    rows <- list()
    
    for (method_name in names(methods_list)) {
      bt <- methods_list[[method_name]]
      
      # Find rebalance periods that fall in the crisis window
      in_crisis <- bt$rebal_ym >= start_ym & bt$rebal_ym <= end_ym
      
      if (sum(in_crisis) == 0) {
        rows[[method_name]] <- data.frame(
          Crisis = crisis_name, Method = method_name,
          Ann_Vol = NA, Ann_Mean = NA, Sharpe = NA,
          stringsAsFactors = FALSE
        )
        next
      }
      
      # Get returns for the crisis holding periods
      crisis_rets <- unlist(bt$oos_returns[in_crisis])
      
      vol    <- sd(crisis_rets) * sqrt(252)
      mn     <- mean(crisis_rets) * 252
      sharpe <- mn / vol
      
      rows[[method_name]] <- data.frame(
        Crisis   = crisis_name,
        Method   = method_name,
        Ann_Vol  = round(vol, 4),
        Ann_Mean = round(mn, 4),
        Sharpe   = round(sharpe, 3),
        stringsAsFactors = FALSE
      )
    }
    
    # Add equal-weight
    if (!is.null(eqw_bt)) {
      in_crisis <- eqw_bt$rebal_ym >= start_ym & eqw_bt$rebal_ym <= end_ym
      
      if (sum(in_crisis) > 0) {
        crisis_rets <- unlist(eqw_bt$oos_returns[in_crisis])
        vol    <- sd(crisis_rets) * sqrt(252)
        mn     <- mean(crisis_rets) * 252
        sharpe <- mn / vol
        
        rows[["1/N"]] <- data.frame(
          Crisis = crisis_name, Method = "1/N",
          Ann_Vol = round(vol, 4), Ann_Mean = round(mn, 4),
          Sharpe = round(sharpe, 3), stringsAsFactors = FALSE
        )
      }
    }
    
    all_results[[crisis_name]] <- do.call(rbind, rows)
  }
  
  do.call(rbind, all_results)
}


# ============================================================
# PLOT 1: Rolling 12-Month Return
# ============================================================
# Shows trailing 12-month compounded return at each month
# Reveals how each method performed recently, without the
# distortion of cumulative charts where early gains inflate the line

plot_rolling_return <- function(methods_list, eqw_bt = NULL,
                                title = "Rolling 12-Month Return",
                                roll_months = 12) {
  
  method_names <- names(methods_list)
  colors       <- c("black", "blue", "red", "darkgreen", "purple", "orange")
  
  # Convert daily OOS to monthly returns, then compute rolling 12-month return
  rolling_rets <- list()
  
  for (name in method_names) {
    monthly <- bt_oos_to_monthly(methods_list[[name]])
    n_months <- length(monthly)
    rr <- rep(NA, n_months)
    
    for (t in roll_months:n_months) {
      window <- monthly[(t - roll_months + 1):t]
      rr[t]  <- prod(1 + window) - 1   # compounded 12-month return
    }
    
    rolling_rets[[name]] <- rr
  }
  
  # Add equal-weight
  if (!is.null(eqw_bt)) {
    monthly <- bt_oos_to_monthly(eqw_bt)
    n_months <- length(monthly)
    rr <- rep(NA, n_months)
    for (t in roll_months:n_months) {
      window <- monthly[(t - roll_months + 1):t]
      rr[t]  <- prod(1 + window) - 1
    }
    rolling_rets[["1/N"]] <- rr
    method_names <- c(method_names, "1/N")
  }
  
  # Y-axis range
  all_vals <- unlist(rolling_rets)
  y_range  <- range(all_vals, na.rm = TRUE)
  
  # Plot
  png("output/rolling_return.png", width = 1000, height = 600)
  
  plot(rolling_rets[[1]], type = "l", col = colors[1], lwd = 2,
       ylim = y_range, xlab = "Month", ylab = "12-Month Return",
       main = title)
  
  abline(h = 0, lty = 2, col = "gray")   # zero line
  
  for (i in 2:length(method_names)) {
    lines(rolling_rets[[method_names[i]]], col = colors[i], lwd = 2)
  }
  
  legend("topright", legend = method_names, col = colors[1:length(method_names)],
         lwd = 2, cex = 0.9, bg = "white")
  
  dev.off()
  cat("  Saved: output/rolling_return.png\n")
}


# ============================================================
# PLOT 2: Bar Chart of Key Metrics
# ============================================================

plot_metric_comparison <- function(summary_table, title = "Performance Comparison") {
  
  png("output/metric_comparison.png", width = 1200, height = 800)
  par(mfrow = c(2, 3), mar = c(5, 4, 3, 1))
  
  methods <- summary_table$Method
  n       <- nrow(summary_table)
  cols    <- c("black", "blue", "red", "darkgreen", "purple", "orange")[1:n]
  
  # Vol
  barplot(summary_table$Ann_Vol, names.arg = methods, col = cols,
          main = "Annualized Vol", ylab = "Vol", las = 2)
  
  # Sharpe
  barplot(summary_table$Sharpe, names.arg = methods, col = cols,
          main = "Sharpe Ratio", ylab = "Sharpe", las = 2)
  
  # Turnover
  barplot(summary_table$Turnover, names.arg = methods, col = cols,
          main = "Annualized Turnover", ylab = "Turnover", las = 2)
  
  # Long Leg
  barplot(summary_table$Long_Leg, names.arg = methods, col = cols,
          main = "Long Leg (Leverage)", ylab = "Sum of Positive Weights", las = 2)
  abline(h = 1, lty = 2, col = "gray")
  
  # Effective N
  barplot(summary_table$Eff_N, names.arg = methods, col = cols,
          main = "Effective N", ylab = "Eff N", las = 2)
  
  # Pred-OOS Gap
  gap_vals <- summary_table$Pred_OOS_Gap
  gap_vals[is.na(gap_vals)] <- 0
  barplot(gap_vals, names.arg = methods, col = cols,
          main = "Pred-OOS Vol Gap", ylab = "Gap (pp)", las = 2)
  
  dev.off()
  cat("  Saved: output/metric_comparison.png\n")
}


# ============================================================
# PLOT 3: Rolling 12-Month Realized Volatility
# ============================================================

plot_rolling_vol <- function(methods_list, eqw_bt = NULL, title = "Rolling Realized Vol",
                             roll_months = 12) {
  
  method_names <- names(methods_list)
  colors       <- c("black", "blue", "red", "darkgreen", "purple", "orange")
  
  # For each method, compute rolling vol over roll_months holding periods
  rolling_vols <- list()
  
  for (name in method_names) {
    oos <- bt_oos_to_monthly(methods_list[[name]])
    # oos is a vector of monthly portfolio returns
    n_months <- length(oos)
    rv <- rep(NA, n_months)
    
    for (t in roll_months:n_months) {
      window <- oos[(t - roll_months + 1):t]
      rv[t]  <- sd(window) * sqrt(12)   # annualize monthly vol
    }
    
    rolling_vols[[name]] <- rv
  }
  
  # Add equal-weight
  if (!is.null(eqw_bt)) {
    oos <- bt_oos_to_monthly(eqw_bt)
    n_months <- length(oos)
    rv <- rep(NA, n_months)
    for (t in roll_months:n_months) {
      window <- oos[(t - roll_months + 1):t]
      rv[t]  <- sd(window) * sqrt(12)
    }
    rolling_vols[["1/N"]] <- rv
    method_names <- c(method_names, "1/N")
  }
  
  # Find y range
  all_vals <- unlist(rolling_vols)
  y_range  <- range(all_vals, na.rm = TRUE)
  
  # Plot
  png("output/rolling_vol.png", width = 1000, height = 600)
  
  plot(rolling_vols[[1]], type = "l", col = colors[1], lwd = 2,
       ylim = y_range, xlab = "Month", ylab = "Annualized Vol",
       main = title)
  
  for (i in 2:length(method_names)) {
    lines(rolling_vols[[method_names[i]]], col = colors[i], lwd = 2)
  }
  
  # Mark crisis periods
  abline(v = NULL)   # placeholder — add vertical lines for crises if you have month indices
  
  legend("topright", legend = method_names, col = colors[1:length(method_names)],
         lwd = 2, cex = 0.9, bg = "white")
  
  dev.off()
  cat("  Saved: output/rolling_vol.png\n")
}


# ============================================================
# HELPER: Convert daily OOS returns to monthly returns
# ============================================================
# Each element of bt$oos_returns is one holding period (roughly one month)
# Sum daily returns within each period to get approximate monthly return

bt_oos_to_monthly <- function(bt) {
  monthly <- sapply(bt$oos_returns, function(daily_rets) {
    # Compound daily returns within the holding period
    prod(1 + daily_rets) - 1
  })
  monthly
}


# ============================================================
# PLOT 4: Weight Heatmap for One Method
# ============================================================
# Shows how portfolio weights evolve over time for a single method
# Useful for visualizing concentration and stability

plot_weight_heatmap <- function(bt, method_name = "Method", top_n = 10) {
  
  # Find the top_n assets by average absolute weight
  avg_abs_w <- colMeans(abs(bt$weights))
  top_assets <- order(avg_abs_w, decreasing = TRUE)[1:top_n]
  
  w_subset <- bt$weights[, top_assets]
  
  png(paste0("output/weights_", gsub(" ", "_", method_name), ".png"),
      width = 1000, height = 600)
  
  # Simple image plot
  image(1:nrow(w_subset), 1:ncol(w_subset), as.matrix(w_subset),
        xlab = "Rebalance Period", ylab = "",
        main = paste("Top", top_n, "Weights Over Time:", method_name),
        axes = FALSE)
  axis(1)
  axis(2, at = 1:top_n, labels = colnames(w_subset), las = 2, cex.axis = 0.8)
  
  dev.off()
  cat("  Saved: output/weights_", method_name, ".png\n")
}


# ============================================================
# TABLE: Crisis period comparison (formatted for printing)
# ============================================================

print_crisis_table <- function(crisis_results) {
  
  crises <- unique(crisis_results$Crisis)
  
  for (cr in crises) {
    cat("\n---", cr, "---\n")
    subset <- crisis_results[crisis_results$Crisis == cr, ]
    subset$Crisis <- NULL
    print(subset, row.names = FALSE)
  }
}

# Robustness diagnostic: mean vs median of per-rebalance metrics
resilience_table <- function(bt) {
  n_rebals <- nrow(bt$weights)
  
  # Per-rebalance metrics
  long_legs  <- rowSums(pmax(bt$weights, 0))
  max_wts    <- apply(bt$weights, 1, max)
  eff_ns     <- 1 / rowSums(bt$weights^2)
  monthly_ret <- sapply(bt$oos_returns, function(r) prod(1 + r) - 1)
  monthly_vol <- sapply(bt$oos_returns, function(r) sd(r) * sqrt(252))
  
  # Turnover between consecutive rebalances
  wt_diffs <- rowSums(abs(diff(bt$weights)))
  
  data.frame(
    Metric      = c("Long Leg", "Max Weight", "Eff N", 
                    "Monthly Return", "Annualized Vol", "Turnover"),
    Mean        = round(c(mean(long_legs), mean(max_wts), mean(eff_ns),
                          mean(monthly_ret), mean(monthly_vol), mean(wt_diffs)), 4),
    Median      = round(c(median(long_legs), median(max_wts), median(eff_ns),
                          median(monthly_ret), median(monthly_vol), median(wt_diffs)), 4),
    Max         = round(c(max(long_legs), max(max_wts), max(eff_ns),
                          max(monthly_ret), max(monthly_vol), max(wt_diffs)), 4),
    Min         = round(c(min(long_legs), min(max_wts), min(eff_ns),
                          min(monthly_ret), min(monthly_vol), min(wt_diffs)), 4),
    stringsAsFactors = FALSE
  )
}

# Run across all methods for a given portfolio type
compare_resilience <- function(methods_list, eqw_bt = NULL) {
  for (name in names(methods_list)) {
    cat("\n---", name, "---\n")
    print(resilience_table(methods_list[[name]]), row.names = FALSE)
  }
  if (!is.null(eqw_bt)) {
    cat("\n--- 1/N ---\n")
    print(resilience_table(eqw_bt), row.names = FALSE)
  }
}