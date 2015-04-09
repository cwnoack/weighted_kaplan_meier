#' Adjusted (weighted) Kaplan-Meier Estimator of the survival function for right censored concentration data
#' 
#' Calculates the AKME of the survival function for right censored data and
#' returns the estimator (S) along with weighted "at risk" and "event" values at
#' each observed (uncensored) concentration. Uses methods from Xie and Liu (2005).
#' @param censored_data data.frame with three columns: (1) measured concentration, (2) flag for nondetects [BDL = 1], (3) unique site identifier.
#' @return
#' @references
Surv_weighted <- function(censored_data){
  check_input(censored_data)
  
  names(censored_data) <- c('Concentration','Censored','Site')
  
  if(!is.factor(censored_data$Site)){
    censored_data$Site <- factor(censored_data$Site)
  }
  
  site_weights <- calc_weights(censored_data$Site)
  
  data_mod <- left_join(censored_data, site_weights, by = 'Site')
  
  data_mod <- arrange(data_mod, desc(Concentration)) %>%
    mutate(Yw = sum(weight) - cumsum(weight) + weight)
  
  observed <- filter(data_mod, Censored == 0) %>%
    select(-Censored)
  
  obs_weight_tab <- group_by(observed, Concentration) %>%
    summarize(dw = sum(weight))  
  
  observed <- left_join(observed, obs_weight_tab, by = "Concentration") %>%
    mutate(P = 1 - dw/Yw) %>%
    filter(!duplicated(Concentration)) %>%
    mutate(S = cumprod(P),
           S = ifelse(S<0,0,S))
  
  return(observed)
}
