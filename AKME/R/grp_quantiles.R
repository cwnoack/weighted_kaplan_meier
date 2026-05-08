#' Weighted quantiles for grouped data
#'
#' Computes [Surv_quantile()] within each level of `Dataset` and pivots
#' the result so that each group has its own column.
#'
#' @param grouped_data A data frame with columns
#'   (1) measured concentration, (2) censoring flag, (3) site identifier,
#'   and (4) `Dataset`.
#' @param percentiles Numeric vector of desired percentiles in `(0, 1)`.
#' @param type Method for quantile estimation; see [Surv_quantile()].
#' @param sig.fig Number of significant figures for the estimate.
#' @return A tibble with column `Percentile` plus one column per level of
#'   `Dataset`, arranged by descending percentile.
#' @seealso [Surv_quantile()], [group_km()].
#' @export
grp_quantiles <- function(grouped_data,
                          percentiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
                          type = "interp",
                          sig.fig = 3) {
  grouped_data |>
    dplyr::group_by(.data$Dataset) |>
    dplyr::group_modify(\(.x, .y) Surv_quantile(
      censored_data = .x,
      percentiles = percentiles,
      type = type,
      sig.fig = sig.fig
    )) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = "Dataset", values_from = "Xh") |>
    dplyr::arrange(dplyr::desc(.data$Percentile))
}
