get_poisson <- function(df_input) {
  
  # - df_input <- backtesting_output$data[[1]]
  prob_H <- unique(df_input[[1]] %>% pull(pred_H))
  prob_A <- unique(df_input[[1]] %>% pull(pred_A))
  
  vec_H <- ((prob_H^c(0:10))*exp(-prob_H))/factorial(c(0:10))
  vec_A <- ((prob_A^c(0:10))*exp(-prob_A))/factorial(c(0:10))
  
  goals_data <- t(t(vec_H)) %*% t(vec_A)
  
  return(as.data.frame(goals_data))
}