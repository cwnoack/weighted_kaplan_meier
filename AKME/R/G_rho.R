#' Weighted log-rank test statistic G_rho
#'
#' Calculates the test statistic `G_rho` for a comparison of two data
#' frames: a *reference* group and a *comparison* group. `rho` is the
#' exponent of the pooled survival weight in the integrand: `rho = 0`
#' gives an unweighted log-rank statistic; `rho = 1` weights toward the
#' largest observations (Singh et al., 2014).
#'
#' @param ref_data Reference-group data: a data frame whose first three
#'   columns are concentration, censoring flag, and site identifier.
#' @param comp_data Comparison-group data, same structure as `ref_data`.
#' @param rho Non-negative real number. Exponent of the pooled survival
#'   weight; `0` yields an unweighted statistic.
#' @return The numeric value of the test statistic `G`.
#' @export
G_rho <- function(ref_data, comp_data, rho = 1) {
  ref_KM  <- Surv_weighted(ref_data)  |> dplyr::select("Concentration", "Yw", "dw")
  comp_KM <- Surv_weighted(comp_data) |> dplyr::select("Concentration", "Yw", "dw")

  comb_KM <- ref_KM |>
    dplyr::full_join(comp_KM, by = "Concentration", suffix = c("_ref", "_comp")) |>
    dplyr::arrange(dplyr::desc(.data$Concentration)) |>
    tidyr::replace_na(list(dw_ref = 0, dw_comp = 0)) |>
    dplyr::mutate(
      Yw_ref  = max(.data$Yw_ref,  na.rm = TRUE) - cumsum(.data$dw_ref)  + .data$dw_ref,
      Yw_comp = max(.data$Yw_comp, na.rm = TRUE) - cumsum(.data$dw_comp) + .data$dw_comp,
      dw_pool = .data$dw_ref + .data$dw_comp,
      Yw_pool = .data$Yw_ref + .data$Yw_comp,
      S_pool  = cumprod(1 - .data$dw_pool / .data$Yw_pool),
      S_pool  = pmax(.data$S_pool, 0),
      sum_arg = .data$S_pool^rho *
        (.data$dw_comp - .data$Yw_comp * (.data$dw_pool / .data$Yw_pool))
    )

  sum(comb_KM$sum_arg)
}
