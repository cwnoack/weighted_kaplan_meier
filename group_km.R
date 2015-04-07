# Calculates the weighted KM estimator for each subgroup
group_km <- function(grouped_data){
  separate_KM <- ddply(grouped_data, .(Dataset),
                       function(df){Surv_weighted(select(df, -Dataset))}) %>%
    tbl_df()
}
