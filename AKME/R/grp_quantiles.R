# Calculate KM quantiles
grp_quantiles <- function(grouped_data,
                          percentiles = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),
                          type = 'interp'){
  quantile_tbl <-grouped_data %>% plyr::ddply(.(Dataset), function(df){
    Surv_quantile(censored_data = dplyr::select(df, -Dataset),
                  percentiles = percentiles, type = type)
  }) %>% tidyr::spread(Dataset, Xh) %>% dplyr::arrange(desc(Percentile))
  return(quantile_tbl)
}
