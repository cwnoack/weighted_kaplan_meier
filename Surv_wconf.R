# Apply Greenwood's formula for S.E. estimation of survival function
Surv_wconf <- function(wKM_object, alpha = 0.05){
  pm <- qnorm(1-alpha/2)
  wKM_object_alt <- wKM_object %>%
    mutate(sqrt_term = sqrt( cumsum( dw/Yw/(Yw - dw) ) ),
           std_err = S * sqrt_term, std_err = ifelse(is.na(std_err),0, std_err),
           UCI = S + pm*std_err, UCI = ifelse(UCI > 1, 1, UCI),
           LCI = S - pm*std_err, LCI = ifelse(LCI <= 0, 0, LCI)) %>%
    select(-sqrt_term)
  return(wKM_object_alt)
}

