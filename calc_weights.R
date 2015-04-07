# This function calculates the weights of each observation as in Singh et al. (2013)
calc_weights <- function(site_vector){
  counts <- data.frame(Site = site_vector) %>%
    group_by(Site) %>% summarise(weight = 1/n())
  return(counts)
}
