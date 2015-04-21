#' Log-rank test statistic G_rho
#' 
#' Calculates the test statistic G_rho for comparison of two data.frames: one with
#' the "reference" data and one with the "comparison" data. Additional input is
#' the value of rho to be used. A value of 1 returns a statistic weighted
#' towards the largest observations (concentrations) by multiplying by a function
#' of the pooled survival estimator, while a value of 0 returns an unweighted statistic.
#' @param ref_data data.frame with three columns: (1) measured concentration, (2) flag for nondetects [BDL = 1], (3) unique site identifier.
#'        comp_data data.frame with three columns: (1) measured concentration, (2) flag for nondetects [BDL = 1], (3) unique site identifier.
#'        rho Positive, real number input. This is the exponential argument of the weighting function as in Singh et al. (2014)
#' @return G real number value of test statistic
#' 
G_rho <- function(ref_data, comp_data, rho = 1){
  ref_KM <- Surv_weighted(ref_data) %>% dplyr::select(Concentration, Yw, dw)
  comp_KM <- Surv_weighted(comp_data) %>% dplyr::select(Concentration, Yw, dw)
  
  comb_KM <- merge(ref_KM, comp_KM, by = 'Concentration', all = T) %>%
    dplyr::arrange(desc(Concentration))
  colnames(comb_KM) <- c('Concentration', 'Yw_ref', 'dw_ref', 'Yw_comp', 'dw_comp')
  
  comb_KM <- comb_KM %>%
    dplyr::mutate(dw_ref = ifelse(is.na(dw_ref),0, dw_ref),
           dw_comp = ifelse(is.na(dw_comp),0, dw_comp),
           Yw_ref = max(Yw_ref, na.rm = T) - cumsum(dw_ref) + dw_ref,
           Yw_comp = max(Yw_comp, na.rm = T) - cumsum(dw_comp) + dw_comp,
           dw_pool = dw_ref + dw_comp,
           Yw_pool = Yw_ref + Yw_comp,
           S_pool = cumprod(1 - dw_pool/Yw_pool),
           S_pool = ifelse(S_pool<0, 0, S_pool),
           sum_arg = S_pool^rho *(dw_comp - Yw_comp*(dw_pool/Yw_pool))
           )
  G <- with(comb_KM, sum(sum_arg))
  return(G)
  
}

## MWE for testing
# N <- 1000
# dummy_dat <- data.frame(Concentration = rlnorm(N, 1, 3),
#                         Censored = rbinom(N, 1, 0.3),
#                         Site = sample(LETTERS[1:6], N, T, 1:6/sum(1:6)),
#                         Dataset = rep(1:2, each = N/2))
# 
# KM1 <- filter(dummy_dat, Dataset == 1)
# KM2 <- filter(dummy_dat, Dataset == 2)
# 
# G_test <- G_rho(KM1, KM2, rho = 0)
# print(G_test)

