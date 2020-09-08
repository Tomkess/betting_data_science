# - Allocation Budget
allocation_budget <- function(i_fr) sum(i_fr)

# - Sharpe Function
sharpe_fct <- function(i_fr){
  
  mean_p <- matrix(i_prob * i_payoff - (1 - i_prob), nrow = 1)
  sd_p <- diag(i_prob * (1 - i_prob) * (i_payoff^2))
  fr <- t(matrix(i_fr, nrow = 1))
  
  return((-1) * as.numeric((mean_p %*% fr)/sqrt(t(fr) %*% sd_p %*% t(t(fr)))))
}
