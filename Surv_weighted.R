# Return the weighted survival estimator
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
