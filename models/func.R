# Function for exponential smoothing with Holt's method, NA values get replaced
# with smoothing predictions
holt_exp_smoothing <- function(vec) {
  require(dplyr)
  
  # Number of observations that need to be forecasted
  n_pred <- sum(is.na(vec))
  non_missing <- vec[1:(length(vec) - n_pred)]
  
  # Define a HoltWinters model
  hw <- HoltWinters(non_missing, beta = F, gamma = F)
  
  # Get the smoothed values for historical observations
  res <- hw %>% 
    .$fitted %>% 
    .[,1] %>% 
    as.numeric(.) %>% 
    c(NA, .)
  
  # Get a simple prediction for the new values
  if (n_pred != 0) {
    res <- c(res, predict(hw, n.ahead = n_pred) %>% as.numeric)
  }
  
  return(round(res, 4))
}