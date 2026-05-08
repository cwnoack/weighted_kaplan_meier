#' AKME for grouped data
#'
#' Calculates the adjusted Kaplan-Meier estimator separately within each
#' level of `Dataset`. Inputs are the same as [Surv_weighted()] plus an
#' additional `Dataset` column.
#'
#' @param grouped_data A data frame with columns
#'   (1) measured concentration, (2) censoring flag, (3) site identifier,
#'   and (4) `Dataset`.
#' @return A tibble with one row per (Dataset, observed concentration)
#'   combination and the AKME columns from [Surv_weighted()].
#' @seealso [Surv_weighted()], [grp_quantiles()].
#' @export
group_km <- function(grouped_data) {
  grouped_data |>
    dplyr::group_by(.data$Dataset) |>
    dplyr::group_modify(\(.x, .y) Surv_weighted(.x)) |>
    dplyr::ungroup()
}
