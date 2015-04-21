#' AKME for grouped data
#' 
#' Calculates the AKME for grouped data. Requires the same input as `Surv_weighted`
#' but with an additional column: "Dataset". Output will be same as `Surv_weighted` but for each group.
#' @param grouped_data data.frame with four columns: (1) measured concentration,
#'  (2) flag for nondetects [BDL = 1],
#'   (3) unique site identifier,
#'    (4) dataset identifier.
#' @return observed A data.frame containing the observed concentrations, the
#'   weighted "at-risk" and "events" at each concentration, and the survival
#'   estimator along with some intermediate data.
group_km <- function(grouped_data){
  separate_KM <- plyr::ddply(grouped_data, .(Dataset),
                       function(df){Surv_weighted(select(df, -Dataset))}) %>%
    dplyr::tbl_df()
  return(separate_KM)
}
