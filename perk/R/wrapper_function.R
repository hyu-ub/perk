# permutation test for pearson or spearman correlation coefficients
perm_cor <- function(x, y, B = 1000, method = c("pearson", "spearman"), alternative = c("two.sided", "less", "greater")) {
  n <- length(x)
  if (method == "spearman") {
    x <- rank(x)
    y <- rank(y)
  }
  rho_c <- cor(x, y, method = "pearson")
  rho_s <- rho_pearson_s_func(x, y) # studentized correlation
  rho_s_star <- replicate(B, rho_pearson_s_func(sample(x), y), simplify = TRUE)
  if (alternative == "two.sided") {
    p_value <- mean(rho_s_star > abs(rho_s) | rho_s_star < -abs(rho_s))
  } else if (alternative == "less") {
    p_value <- mean(rho_s_star < abs(rho_s))
  } else if (alternative == "greater") {
    p_value <- mean(rho_s_star > rho_s)
  }
  res <- list(estimate = rho_c, p.value = p_value, method = method, alternative = alternative)
  return(res)
}

# permutation test for ccc
perm_ccc <- function(x, y, B=1000, alternative = c("two.sided", "less", "greater")) {
  n <- length(x)
  rho_c <- rho_ccc_func(x, y)
  rho_s <- rho_ccc_s_func(x, y)
  rho_s_star <- replicate(B, rho_ccc_s_func(sample(x), y), simplify = TRUE)
  if (alternative == "two.sided") {
    p_value <- mean(rho_s_star > abs(rho_s) | rho_s_star < -abs(rho_s))
  } else if (alternative == "less") {
    p_value <- mean(rho_s_star < abs(rho_s))
  } else if (alternative == "greater") {
    p_value <- mean(rho_s_star > rho_s)
  }
  res <- list(estimate = rho_c, p.value = p_value, method = "ccc", alternative = alternative)
  return(res)
}

