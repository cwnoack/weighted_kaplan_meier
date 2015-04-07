Surv_quantile <- function(censored_data,
                          percentiles = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)){
  weighted_km <- Surv_weighted(censored_data) %>% select(Concentration, S)
  fl.h <- function(percentile, S){
    h.temp <- which.min(abs(S - percentile))
    if(S[h.temp]>percentile) {h.temp <- h.temp - 1}
    return(h.temp)
  }
  h.low <- sapply(percentiles, function(p) fl.h(p, weighted_km$S))
  h.high <- h.low + 1
  perc_df <- data.frame(percentiles, h.low, h.high) %>%
    mutate(Q = approx(x = weighted_km[c(h.low, h.high),'S'],
                      y = weighted_km[c(h.low, h.high),'Concentration'],
                      xout = percentiles)$y)
}
