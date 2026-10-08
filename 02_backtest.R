# 02_backtest.R

run_backtest <- function(R, ym, mkt, estimator_fn, window_days = 252) {
  
  R <- as.matrix(R)
  n_obs    <- nrow(R)
  n_assets <- ncol(R)
  ones     <- rep(1, n_assets)
  
  month_starts  <- which(!duplicated(ym))
  rebalance_idx <- month_starts[month_starts > window_days]
  n_rebals      <- length(rebalance_idx)
  
  # Storage — GMVP
  w_gmvp_mat    <- matrix(NA, nrow = n_rebals, ncol = n_assets)
  colnames(w_gmvp_mat) <- colnames(R)
  pred_var_gmvp <- numeric(n_rebals)
  oos_ret_gmvp  <- vector("list", n_rebals)
  drifted_gmvp_mat <- matrix(NA, nrow = n_rebals, ncol = n_assets)
  
  # Storage — Max beta/vol
  w_maxbv_mat    <- matrix(NA, nrow = n_rebals, ncol = n_assets)
  colnames(w_maxbv_mat) <- colnames(R)
  pred_var_maxbv <- numeric(n_rebals)
  oos_ret_maxbv  <- vector("list", n_rebals)
  drifted_maxbv_mat <- matrix(NA, nrow = n_rebals, ncol = n_assets)
  
  rebal_ym <- ym[rebalance_idx]
  
  for (i in seq_along(rebalance_idx)) {
    t <- rebalance_idx[i]
    
    est_rows   <- (t - window_days):(t - 1)
    est_window <- R[est_rows, ]
    m_window   <- mkt[est_rows]
    
    if (length(formals(estimator_fn)) == 1) {
      Sigma_hat <- estimator_fn(est_window)
    } else {
      Sigma_hat <- estimator_fn(est_window, m_window)
    }
    
    # GMVP: w ∝ Σ⁻¹·1
    Sinv_ones  <- solve(Sigma_hat, ones)
    w_gmvp     <- Sinv_ones / sum(Sinv_ones)
    
    # Max beta/vol: w ∝ Σ⁻¹·β
    betas      <- as.vector(cov(est_window, m_window) / var(m_window))
    Sinv_beta  <- solve(Sigma_hat, betas)
    w_maxbv    <- Sinv_beta / sum(Sinv_beta)
    
    # Holding period
    t_next    <- if (i < n_rebals) rebalance_idx[i + 1] else n_obs + 1
    hold_rows <- t:(t_next - 1)
    hold_returns_mat <- 1 + R[hold_rows, , drop = FALSE]
    
    # GMVP storage
    w_gmvp_mat[i, ]    <- w_gmvp
    pred_var_gmvp[i]   <- as.numeric(t(w_gmvp) %*% Sigma_hat %*% w_gmvp)
    oos_ret_gmvp[[i]]  <- as.numeric(R[hold_rows, , drop = FALSE] %*% w_gmvp)
    
    grown_gmvp <- w_gmvp * apply(hold_returns_mat, 2, prod)
    drifted_gmvp_mat[i, ] <- grown_gmvp / sum(grown_gmvp)
    
    # Max beta/vol storage
    w_maxbv_mat[i, ]    <- w_maxbv
    pred_var_maxbv[i]   <- as.numeric(t(w_maxbv) %*% Sigma_hat %*% w_maxbv)
    oos_ret_maxbv[[i]]  <- as.numeric(R[hold_rows, , drop = FALSE] %*% w_maxbv)
    
    grown_maxbv <- w_maxbv * apply(hold_returns_mat, 2, prod)
    drifted_maxbv_mat[i, ] <- grown_maxbv / sum(grown_maxbv)
  }
  
  list(
    gmvp = list(
      rebal_ym    = rebal_ym,
      weights     = w_gmvp_mat,
      drifted_weights = drifted_gmvp_mat,
      pred_var    = pred_var_gmvp,
      oos_returns = oos_ret_gmvp
    ),
    maxbv = list(
      rebal_ym    = rebal_ym,
      weights     = w_maxbv_mat,
      drifted_weights = drifted_maxbv_mat,
      pred_var    = pred_var_maxbv,
      oos_returns = oos_ret_maxbv
    )
  )
}

summarize_backtest <- function(bt) {
  all_oos <- unlist(bt$oos_returns)
  
  ann_mean     <- mean(all_oos) * 252
  ann_vol      <- sd(all_oos)   * sqrt(252)
  pred_ann_vol <- mean(sqrt(bt$pred_var), na.rm = TRUE) * sqrt(252)
  
  sharpe       <- ann_mean / ann_vol
  pred_oos_gap <- ann_vol - pred_ann_vol
  
  if (!is.null(bt$drifted_weights)) {
    n_rebals <- nrow(bt$weights)
    rebal_trades <- abs(bt$weights[-1, ] - bt$drifted_weights[-n_rebals, ])
    turnover <- mean(rowSums(rebal_trades)) * 12
  } else {
    turnover <- mean(rowSums(abs(diff(bt$weights)))) * 12
  }
  
  long_leg     <- mean(rowSums(pmax(bt$weights, 0)))
  max_weight   <- mean(apply(bt$weights, 1, max))
  eff_n        <- mean(1 / rowSums(bt$weights^2))
  
  list(
    ann_mean     = ann_mean,
    ann_vol      = ann_vol,
    pred_ann_vol = pred_ann_vol,
    sharpe       = sharpe,
    pred_oos_gap = pred_oos_gap,
    turnover     = turnover,
    long_leg     = long_leg,
    max_weight   = max_weight,
    eff_n        = eff_n
  )
}

run_equal_weight <- function(R, ym, window_days = 252) {
  R <- as.matrix(R)
  n_obs    <- nrow(R)
  n_assets <- ncol(R)
  
  month_starts  <- which(!duplicated(ym))
  rebalance_idx <- month_starts[month_starts > window_days]
  n_rebals      <- length(rebalance_idx)
  
  weights_mat <- matrix(1/n_assets, nrow = n_rebals, ncol = n_assets)
  colnames(weights_mat) <- colnames(R)
  
  pred_var    <- rep(NA_real_, n_rebals)
  oos_returns <- vector("list", n_rebals)
  drifted_mat <- matrix(NA, nrow = n_rebals, ncol = n_assets)
  rebal_ym    <- ym[rebalance_idx]
  
  w <- rep(1/n_assets, n_assets)
  
  for (i in seq_along(rebalance_idx)) {
    t <- rebalance_idx[i]
    t_next <- if (i < n_rebals) rebalance_idx[i + 1] else n_obs + 1
    hold_rows <- t:(t_next - 1)
    oos_returns[[i]] <- as.numeric(R[hold_rows, , drop = FALSE] %*% w)
    
    hold_returns_mat <- 1 + R[hold_rows, , drop = FALSE]
    grown <- w * apply(hold_returns_mat, 2, prod)
    drifted_mat[i, ] <- grown / sum(grown)
  }
  
  list(
    rebal_ym    = rebal_ym,
    weights     = weights_mat,
    drifted_weights = drifted_mat,
    pred_var    = pred_var,
    oos_returns = oos_returns
  )
}