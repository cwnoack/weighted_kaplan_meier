#' Weighted quantiles for censored data
#' 
#' Calculates the AKME for censored data and returns a table of desired quantiles. 
#'  Output contains the desired percentiles and the corresponding order statistics.
#'  Code will throw warnings if one or more of the desired percentiles is less than 
#'  the fraction of censored data and may produce NA values for order statistics at 
#'  those percentiles depending on the `type` argument to the function 
#'  (i.e. `nearest` will not return NA but may return repeated values).
#' @param censored_data data.frame with three columns: (1) measured concentration,
#'  (2) flag for nondetects [BDL = 1], and
#'   (3) unique site identifier
#' @param percentiles vector of desired percentiles
#' @param type method for calculation of quantiles, either: 
#' (1) 'interp' for interpolation between nearest points,
#' (2) 'PiR' for nearest value that does not exceed the percentile, or
#' (3) 'nearest' for nearest value (that may exceed desired percentile)
#' @param sig.fig desired number of significant figures for estimate.
#' @return perc_df data.frame containing the desired percentiles and the 
#' order statistic estimates of those percentiles from `censored_data`
Surv_quantile <- function(censored_data,
                          percentiles = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),
                          type = 'interp',
                          sig.fig = 3){
  cenfrac <- mean(as.logical(censored_data$Censored))
  
  weighted_km <- Surv_weighted(censored_data) %>% dplyr::select(Concentration, S)
  
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
                  dplyr::mutate(Xh = weighted_km[h.low,'Concentration'])},
         nearest = {perc_df <- perc_df %>%
                      dplyr::mutate(Xh = weighted_km[h.low,'Concentration'])})
  
  perc_df <- perc_df %>% dplyr::arrange(desc(Percentile)) %>% 
    dplyr::select(Percentile, Xh) %>%
    dplyr::mutate(Xh = signif(Xh, sig.fig))
  return(perc_df)
}
