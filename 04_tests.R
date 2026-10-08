# 04_tests.R
# Statistical tests for comparing portfolio methods
# Requires: library(PeerPerformance)

# =============================================================
# TEST 1: Paired t-test on realized daily squared returns
# =============================================================
# Tests H0: method A and method B have the same realized variance
# Input: two vectors of daily portfolio returns (same length, same dates)

paired_var_test <- function(ret_A, ret_B, label_A = "A", label_B = "B") {
  
  # Squared returns = daily realized variance proxy
  sq_A <- ret_A^2
  sq_B <- ret_B^2
  
  # Paired difference: positive means A has higher variance
  d <- sq_A - sq_B
  
  result <- t.test(d)
  
  list(
    label        = paste(label_A, "vs", label_B),
    mean_diff    = result$estimate,
    t_stat       = result$statistic,
    p_value      = result$p.value,
    conf_int     = result$conf.int,
    conclusion   = ifelse(result$p.value < 0.05,
                          paste(label_A, "has significantly different variance from", label_B),
                          "No significant difference in variance")
  )
}


# =============================================================
# TEST 2: Block bootstrap confidence intervals
# =============================================================
# Resamples holding periods in blocks, recomputes performance metrics
# Input: backtest result (output of run_backtest$gmvp or similar)

block_bootstrap <- function(bt, n_boot = 1000, block_size = 6, seed = 42) {
  
  set.seed(seed)
  n_rebals <- length(bt$oos_returns)
  
  # Storage for bootstrap samples
  boot_vol     <- numeric(n_boot)
  boot_mean    <- numeric(n_boot)
  boot_sharpe  <- numeric(n_boot)
  boot_turnover <- numeric(n_boot)
  
  for (b in 1:n_boot) {
    
    # Draw block starting points
    n_blocks    <- ceiling(n_rebals / block_size)
    block_starts <- sample(1:(n_rebals - block_size + 1), n_blocks, replace = TRUE)
    
    # Build resampled indices from blocks
    boot_idx <- integer(0)
    for (s in block_starts) {
      boot_idx <- c(boot_idx, s:(s + block_size - 1))
    }
    boot_idx <- boot_idx[1:n_rebals]   # trim to original length
    
    # Resample returns and weights
    boot_ret <- unlist(bt$oos_returns[boot_idx])
    boot_wts <- bt$weights[boot_idx, ]
    
    # Compute metrics on resampled data
    boot_vol[b]    <- sd(boot_ret) * sqrt(252)
    boot_mean[b]   <- mean(boot_ret) * 252
    boot_sharpe[b] <- boot_mean[b] / boot_vol[b]
    boot_turnover[b] <- mean(rowSums(abs(diff(boot_wts)))) * 12
  }
  
  # 95% confidence intervals
  ci <- function(x) quantile(x, probs = c(0.025, 0.975))
  
  list(
    vol     = list(mean = mean(boot_vol),     ci = ci(boot_vol)),
    ann_ret = list(mean = mean(boot_mean),    ci = ci(boot_mean)),
    sharpe  = list(mean = mean(boot_sharpe),  ci = ci(boot_sharpe)),
    turnover = list(mean = mean(boot_turnover), ci = ci(boot_turnover))
  )
}


# =============================================================
# TEST 3: Ledoit-Wolf test for equality of Sharpe ratios
# =============================================================
# Uses HAC standard errors to account for fat tails and autocorrelation
# Input: two vectors of daily portfolio returns

sharpe_equality_test <- function(ret_A, ret_B, label_A = "A", label_B = "B") {
  
  result <- PeerPerformance::sharpeTesting(ret_A, ret_B)
  
  p_val <- result$pval
  sh_A  <- result$sharpe[1] * sqrt(252)
  sh_B  <- result$sharpe[2] * sqrt(252)
  
  list(
    label   = paste(label_A, "vs", label_B),
    sharpe_A = sh_A,
    sharpe_B = sh_B,
    diff     = sh_A - sh_B,
    p_value  = p_val,
    conclusion = ifelse(p_val < 0.05,
                        "Sharpe ratios are significantly different",
                        "Cannot reject equal Sharpe ratios")
  )
}


# =============================================================
# HELPER: Run all pairwise tests across a list of methods
# =============================================================
# Input: named list of backtest results, e.g.
#   methods <- list(
#     "Sample"  = bt_sample$gmvp,
#     "LW-CC"   = bt_lw_cc$gmvp,
#     "LW-Idx"  = bt_lw_idx$gmvp
#   )

run_all_tests <- function(methods) {
  
  names_list <- names(methods)
  n_methods  <- length(methods)
  
  # Flatten returns for each method
  rets <- lapply(methods, function(bt) unlist(bt$oos_returns))
  
  # Ensure all return vectors are the same length
  min_len <- min(sapply(rets, length))
  rets    <- lapply(rets, function(r) r[1:min_len])
  
  cat("=== Paired t-tests (variance) ===\n\n")
  for (i in 1:(n_methods - 1)) {
    for (j in (i + 1):n_methods) {
      res <- paired_var_test(rets[[i]], rets[[j]], names_list[i], names_list[j])
      cat(res$label, "\n")
      cat("  Mean diff in sq returns:", round(res$mean_diff, 8), "\n")
      cat("  t-stat:", round(res$t_stat, 3), " p-value:", round(res$p_value, 4), "\n")
      cat("  ", res$conclusion, "\n\n")
    }
  }
  
  cat("=== Ledoit-Wolf Sharpe ratio tests ===\n\n")
  for (i in 1:(n_methods - 1)) {
    for (j in (i + 1):n_methods) {
      res <- sharpe_equality_test(rets[[i]], rets[[j]], names_list[i], names_list[j])
      cat(res$label, "\n")
      cat("  Sharpe A:", round(res$sharpe_A, 3), " Sharpe B:", round(res$sharpe_B, 3), "\n")
      cat("  p-value:", round(res$p_value, 4), "\n")
      cat("  ", res$conclusion, "\n\n")
    }
  }
  
  cat("=== Bootstrap confidence intervals ===\n\n")
  for (i in 1:n_methods) {
    cat(names_list[i], "\n")
    boot <- block_bootstrap(methods[[i]])
    cat("  Vol:     ", round(boot$vol$mean, 4),
        " [", round(boot$vol$ci[1], 4), ",", round(boot$vol$ci[2], 4), "]\n")
    cat("  Sharpe:  ", round(boot$sharpe$mean, 3),
        " [", round(boot$sharpe$ci[1], 3), ",", round(boot$sharpe$ci[2], 3), "]\n")
    cat("  Turnover:", round(boot$turnover$mean, 2),
        " [", round(boot$turnover$ci[1], 2), ",", round(boot$turnover$ci[2], 2), "]\n\n")
  }
}