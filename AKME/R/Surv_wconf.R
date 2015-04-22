#' Apply Greenwood's formula for S.E. estimation of survival function
#' @export
#' @import plyr
#' @import dplyr
Surv_wconf <- function(wKM_object, alpha = 0.05){
  pm <- qnorm(1-alpha/2)
  wKM_object_alt <- wKM_object %>%
    mutate(ratio_term = dw/Yw/(Yw - dw),
           ratio_term = ifelse(ratio_term == Inf, 0, ratio_term),
           sqrt_term = sqrt( ratio_term ),
           std_err = S * sqrt_term, std_err = ifelse(is.na(std_err),0, std_err),
           UCI = S + pm*std_err, UCI = ifelse(UCI > 1, 1, UCI),
           LCI = S - pm*std_err, LCI = ifelse(LCI <= 0, 0, LCI)) %>%
    select(-sqrt_term, -ratio_term)
  return(wKM_object_alt)
}

# # MWE for replication
# 
# N <- 1000
# dummy_dat <- data.frame(Concentration = rlnorm(N, 1, 3),
#                           Censored = rbinom(N, 1, 0.3),
#                           Site = sample(LETTERS[1:4], N, T))
# 
# KM <- Surv_weighted(dummy_dat)
# 
# KM_wconf <- KM %>% Surv_wconf() %>% tbl_df()
# 
# print(KM_wconf)
# 
# plt <- ggplot(KM_wconf, aes(x = Concentration, y = S)) +
#   geom_step(aes(y = UCI), color = 'red', linetype = 1) +
#   geom_step(aes(y = LCI), color = 'red', linetype = 1) +
#   geom_step() + scale_x_log10() +theme_classic()
# 
# print(plt)
