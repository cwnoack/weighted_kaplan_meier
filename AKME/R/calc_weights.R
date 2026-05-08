#' Calculate site-specific observational weights as 1/n
#'
#' For pooled data from many sites (or any equivalent sampling group),
#' calculate the weight `1 / n_k` of an individual observation from site `k`.
#'
#' @param site_vector A vector of site identifiers.
#' @return A tibble with columns `Site` and `weight`.
#' @export
#' @examples
#' sites <- sample(letters[1:10], 100, replace = TRUE,
#'                 prob = 1:10 / sum(1:10))
#' calc_weights(sites)
calc_weights <- function(site_vector) {
  tibble::tibble(Site = site_vector) |>
    dplyr::group_by(.data$Site) |>
    dplyr::summarise(weight = 1 / dplyr::n(), .groups = "drop")
}
