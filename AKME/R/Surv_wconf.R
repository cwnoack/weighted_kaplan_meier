#' Greenwood-style confidence band for the weighted Kaplan-Meier estimator
#'
#' Adds Greenwood standard errors and approximate `(1 - alpha)` confidence
#' bands to the output of [Surv_weighted()].
#'
#' @param wKM_object Output of [Surv_weighted()].
#' @param alpha Type-I error rate. Default `0.05` for a 95% interval.
#' @return A tibble with the columns of `wKM_object` plus `std_err`, `UCI`,
#'   and `LCI`.
#' @export
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   Concentration = rlnorm(50, 1, 1),
#'   Censored = rbinom(50, 1, 0.2),
#'   Site = sample(letters[1:3], 50, replace = TRUE)
#' )
#' Surv_wconf(Surv_weighted(dat))
Surv_wconf <- function(wKM_object, alpha = 0.05) {
  pm <- stats::qnorm(1 - alpha / 2)
  wKM_object |>
    dplyr::mutate(
      ratio_term = .data$dw / .data$Yw / (.data$Yw - .data$dw),
      ratio_term = dplyr::if_else(is.finite(.data$ratio_term), .data$ratio_term, 0),
      std_err = .data$S * sqrt(.data$ratio_term),
      std_err = tidyr::replace_na(.data$std_err, 0),
      UCI = pmin(.data$S + pm * .data$std_err, 1),
      LCI = pmax(.data$S - pm * .data$std_err, 0)
    ) |>
    dplyr::select(-"ratio_term")
}
