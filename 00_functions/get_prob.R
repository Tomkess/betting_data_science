get_probs <- function(df_input){
  
  mat_input <- as.matrix(df_input[[1]])
  # mat_input <- as.matrix(backtesting_output$poisson_data[[1]])
  
  prob_25 <- 
    mat_input[1,1] + mat_input[1,2] + mat_input[1,3] +
    mat_input[2,1] + mat_input[2,2] +
    mat_input[3,1]
  prob_251 <- 1 - prob_25
  
  prob_h <- sum(sum(lower.tri(mat_input, diag = F) * mat_input))
  prob_a <- sum(sum(upper.tri(mat_input, diag = F) * mat_input))
  prob_d <- 1 - prob_h - prob_a
  
  return(data.frame("prob_25" = prob_25,
                    "prob_251" = prob_251,
                    "prob_h" = prob_h,
                    "prob_a" = prob_a,
                    "prob_d" = prob_d))
}