# Function for calculating quantiles of distributions using AKME
Surv_quantile <- function(censored_data,
                          percentiles = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),
                          type = 'interp'){
  cenfrac <- mean(censored_data$Censored)
  
  weighted_km <- Surv_weighted(censored_data) %>% select(Concentration, S)
  
  if(any(cenfrac > percentiles)){
    warning(paste('The fraction of censored data is larger ',
                  'than one or more desired percentiles.\n',
                  'This may produce NA values or unreliable estimates.', sep = ''))
  }
  
  if(any(weighted_km$S < min(percentiles))){
    warning('Minimum desired percentile below minimum calculated from data, may produce NA values.')
  }
  
  fl.h <- function(percentile, S){
    h.temp <- which.min(abs(S - percentile))
    if(type != 'nearest' && S[h.temp]>percentile) {h.temp <- h.temp + 1}
    return(h.temp)
  }
  
  h.low <- sapply(percentiles, function(p) fl.h(p, weighted_km$S))
  h.high <- h.low - 1
  perc_df <- data.frame(Percentile = percentiles, h.low, h.high)
  
  switch(type,
         interp = {perc_df <- perc_df %>%
                     mutate(Xh = approx(x = weighted_km[c(h.low, h.high),'S'],
                                        y = weighted_km[c(h.low, h.high),'Concentration'],
                                        xout = Percentile)$y)},
         PiR = {perc_df <- perc_df %>%
                  mutate(Xh = weighted_km[h.low,'Concentration'])},
         nearest = {perc_df <- perc_df %>%
                      mutate(Xh = weighted_km[h.low,'Concentration'])})
  
  perc_df <- perc_df %>% arrange(desc(Percentile)) %>% select(Percentile, Xh)
  return(perc_df)
}
