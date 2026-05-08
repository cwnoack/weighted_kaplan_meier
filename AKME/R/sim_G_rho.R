#' Simulate a single value of the log-rank statistic under the null
#'
#' Generates one realisation of `G_rho` under the null hypothesis of no
#' group difference by reshuffling the `Dataset` labels via permutation,
#' bootstrap resampling, or simulation from the marginal class proportions.
#' Used internally by [log_rank()] to build a reference distribution.
#'
#' @param grouped_data A data frame with columns `Concentration`,
#'   `Censored`, `Site`, and `Dataset` (the group identifier).
#' @param rho Non-negative real number passed through to [G_rho()].
#' @param method One of `"perm"` (sample without replacement),
#'   `"boot"` (sample with replacement), or `"sim"` (sample from the
#'   marginal class proportions).
#' @return A single numeric value of the simulated `G_rho` statistic.
#' @export
sim_G_rho <- function(grouped_data, rho = 1, method = "perm") {
  method <- rlang::arg_match(method, c("perm", "boot", "sim"))

  ds_factor <- factor(grouped_data$Dataset)
  data_levels <- levels(ds_factor)
  grouped_data$Dataset <- ds_factor

  p <- prop.table(table(ds_factor))

  new_labs <- switch(
    method,
    sim  = sample(data_levels, size = nrow(grouped_data),
                  replace = TRUE, prob = p),
    boot = sample(grouped_data$Dataset, replace = TRUE),
    perm = sample(grouped_data$Dataset, replace = FALSE)
  )

  grouped_data$Dataset <- new_labs

  G1 <- grouped_data |>
    dplyr::filter(.data$Dataset == data_levels[1]) |>
    dplyr::select(-"Dataset")
  G2 <- grouped_data |>
    dplyr::filter(.data$Dataset == data_levels[2]) |>
    dplyr::select(-"Dataset")

  G_rho(G1, G2, rho)
}
