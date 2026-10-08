# 03_estimators.R

# Helper: compute optimal shrinkage intensity δ̂ for any target F
compute_delta <- function(X, S, F, T_obs) {
  # X: T×N demeaned returns
  # S: sample covariance (from X)
  # F: shrinkage target
  # T_obs: number of observations
  
  # α̂² — variance of sample covariance
  alpha_sq <- 0
  for (t in 1:T_obs) {
    diff_t   <- tcrossprod(X[t, ]) - S
    alpha_sq <- alpha_sq + sum(diff_t^2)
  }
  alpha_sq <- alpha_sq / T_obs^2
  
  # β̂² — squared bias of target, with correction
  beta_sq <- sum((F - S)^2)
  beta_sq <- max(beta_sq - alpha_sq, 0)
  
  # Optimal δ, clipped to [0,1]
  delta <- alpha_sq / (alpha_sq + beta_sq)
  delta <- min(max(delta, 0), 1)
  
  delta
}

# Shrink to constant correlation matrix
lw_constant_corr <- function(R) {
  R <- as.matrix(R)
  T_obs <- nrow(R)
  N     <- ncol(R)
  
  # Demean and compute sample covariance
  X <- scale(R, center = TRUE, scale = FALSE)
  S <- crossprod(X) / T_obs
  
  # Extract standard deviations and correlation matrix
  s_vec <- sqrt(diag(S))                          # N individual std devs
  C     <- S / (s_vec %o% s_vec)                  # sample correlation matrix
  
  # Average off-diagonal correlation
  rho_bar <- (sum(C) - N) / (N * (N - 1))
  
  # Build constant-correlation target
  C_bar <- matrix(rho_bar, N, N)
  diag(C_bar) <- 1
  D_half <- diag(s_vec)
  F <- D_half %*% C_bar %*% D_half
  
  # Compute optimal δ and shrink
  delta <- compute_delta(X, S, F, T_obs)
  
  delta * F + (1 - delta) * S
}

# Shrink to index-model-implied covariance
# mkt_full is the full market return vector; pass window rows from backtest
lw_index_model <- function(R, m) {
  R <- as.matrix(R)
  T_obs <- nrow(R)
  N     <- ncol(R)
  
  # Demean and compute sample covariance
  X <- scale(R, center = TRUE, scale = FALSE)
  S <- crossprod(X) / T_obs
  
  # Index model: R_i = α_i + β_i * R_m + ε_i
  betas     <- as.vector(cov(R, m) / var(m))
  sigma2_m  <- var(m)
  residuals <- R - m %o% betas                    # T×N residual matrix
  sigma2_eps <- colMeans(residuals^2)              # residual variances
  # Correct: use demeaned residuals for variance
  sigma2_eps <- diag(cov(residuals)) * (T_obs - 1) / T_obs
  
  # Build index-model target: F = σ²_m * β β' + D_ε
  F <- sigma2_m * (betas %*% t(betas)) + diag(sigma2_eps)
  
  # Compute optimal δ and shrink
  delta <- compute_delta(X, S, F, T_obs)
  
  delta * F + (1 - delta) * S
}

# GARCH(1,1) covariance estimator
# Time-varying variances via univariate GARCH, static sample correlations
# Requires: library(rugarch)

garch_cov <- function(R) {
  R <- as.matrix(R)
  T_obs <- nrow(R)
  N     <- ncol(R)
  
  # Step 1: sample correlation matrix (static)
  S <- cov(R)
  C <- cov2cor(S)
  
  # Step 2: fit GARCH(1,1) to each asset, extract final conditional variance
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = "norm"
  )
  
  garch_var <- numeric(N)
  
  for (j in 1:N) {
    fit <- tryCatch(
      ugarchfit(spec, data = R[, j], solver = "hybrid"),
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      # Fallback: use sample variance if GARCH fails to converge
      garch_var[j] <- var(R[, j])
    } else {
      # Last conditional variance = forecast for next period
      garch_var[j] <- tail(sigma(fit)^2, 1)
    }
  }
  
  # Step 3: build covariance matrix
  # V = D^(1/2) · C · D^(1/2)
  s_vec  <- sqrt(garch_var)
  D_half <- diag(s_vec)
  Sigma  <- D_half %*% C %*% D_half
  
  Sigma
}