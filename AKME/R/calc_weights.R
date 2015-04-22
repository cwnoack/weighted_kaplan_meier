#' Calculate the site-specific observational weights as 1/n
#' 
#' For pooled data from many sites, or other equivalent sampling groups,
#' calculate the weight of an individual observation from that site.
#' @importFrom magrittr "%>%"
#' @import plyr
#' @import dplyr
#' @export
#' @param site_vector A vector of site IDs
#' @examples
#' sites <- sample(letters[1:10],100,replace = T, prob = 1:10/sum(1:10))
#' weights <- calc_weights(sites)

calc_weights <- function(site_vector){
  counts <- data.frame(Site = site_vector) %>%
    dplyr::group_by(Site) %>% dplyr::summarise(weight = 1/n())
  return(counts)
}
