#' Weighted quantiles for grouped data
#' 
#' Calculates the AKME for grouped data and returns a table of desired quantiles.
#'  Requires the same input as `Surv_quanitle` but with an additional column:
#'   "Dataset". Output will be same as `Surv_quantile` but for each group.
#' @importFrom magrittr "%>%"
#' @import plyr
#' @import dplyr
#' @export
#' @param grouped_data data.frame with four columns: (1) measured concentration,
#'  (2) flag for nondetects [BDL = 1],
#'   (3) unique site identifier,
#'    (4) dataset identifier.
#' @param percentiles vector of desired percentiles
#' @param type method for calculation of quantiles, either: 
#' (1) 'interp' for interpolation between nearest points,
#' (2) 'PiR' for nearest value that does not exceed the percentile, or
#' (3) 'nearest' for nearest value (that may exceed desired percentile)
#' @param sig.fig desired number of significant figures for estimate.
#' @return quantile_tbl data.frame containing the desired percentiles and the 
#' order statistic estimates of those percentiles from each group in `grouped_data`
grp_quantiles <- function(grouped_data,
                          percentiles = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),
                          type = 'interp', sig.fig = 3){
  quantile_tbl <-grouped_data %>% plyr::ddply(.(Dataset), function(df){
    Surv_quantile(censored_data = dplyr::select(df, -Dataset),
                  percentiles = percentiles, type = type, sig.fig = sig.fig)
  }) %>% tidyr::spread(Dataset, Xh) %>% dplyr::arrange(desc(Percentile))
  return(quantile_tbl)
}
