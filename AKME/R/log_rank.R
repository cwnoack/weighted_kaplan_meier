#' Log-rank test
#' 
#' Weighted or unweighted log-rank test of differences between two groups using
#' the methodologies of Xie and Liu (2005) and Singh et al. (2014). This is a 
#' non-parametric test that compares a true test statistics to a number of
#' randomized statistics (randomized by simulation or one of two resampling 
#' strategies). A P-value is calculated based on the extremity of the true statistic
#' compared to the randomized statistics.
#' @param grouped_data data.frame with four columns: (1) measured concentration,
#'  (2) flag for nondetects [BDL = 1],
#'   (3) unique site identifier,
#'    (4) dataset identifier.
#'    comp_group identifier of the "comparison" group for the test. Defaults to 
#'    the second group if no value is supplied.
#'    rho Positive, real number input. This is the exponential argument of the weighting function as in Singh et al. (2014)
#'    method method for randomization of groupings for permutation test. Either `perm` for resampling without replacement, `boot` for resampling with replacement, or `sim` for simulation from a binomial distribution.
#' @return observed A data.frame containing the observed concentrations, the
#'   weighted "at-risk" and "events" at each concentration, and the survival
#'   estimator along with some intermediate data.
log_rank <- function(grouped_data, comp_group = NULL, rho = 1,
                     method = 'perm', boots = 1000, alternative = 'two.sided'){
  if(is.null(comp_group)){
    
    grps <- levels(factor(grouped_data$Dataset))
    ref <- dplyr::filter(grouped_data, Dataset == grps[1]) %>% 
      dplyr::select(-Dataset)
    comp <- dplyr::filter(grouped_data, Dataset == grps[2]) %>% 
      dplyr::select(-Dataset)
    
  } else {
    
    ref <- dplyr::filter(grouped_data, Dataset != comp_group) %>%
      dplyr::select(-Dataset)
    comp <- dplyr::filter(grouped_data, Dataset == comp_group) %>%
      dplyr::select(-Dataset)
    
  }
  
  test_G <- G_rho(ref_data = ref, comp_dat = comp, rho = rho)
  sim_G <- replicate(boots, sim_G_rho(grouped_data, rho, method))
  
  switch(alternative,
         two.sided = {p.val <- sum(abs(sim_G) >= abs(test_G))/boots},
         greater = {p.val <- sum(sim_G >= test_G)/boots},
         less = {p.val <- sum(sim_G <= test_G)/boots}
         )
  
  output <- list(G_test = test_G, boot_G = sim_G, p.val = p.val, rho = rho)
  
  return(output)
  
}
