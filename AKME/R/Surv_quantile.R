#' Weighted quantiles for right-censored data
#'
#' Calculates the AKME for censored data and returns a table of desired
#' quantiles. Output contains the desired percentiles and the corresponding
#' order-statistic estimates. Warnings are emitted if a desired percentile
#' falls below the censored fraction (which may produce `NA` values
#' depending on `type`).
#'
#' @param censored_data A data frame whose first three columns are
#'   (1) the measured concentration, (2) a censoring flag, and
#'   (3) a unique site identifier.
#' @param percentiles Numeric vector of desired percentiles in `(0, 1)`.
#' @param type Method for quantile estimation: `"interp"` (linear
#'   interpolation between nearest points), `"PiR"` (largest order
#'   statistic that does not exceed the percentile), or `"nearest"`
#'   (nearest order statistic, possibly exceeding the percentile).
#' @param sig.fig Number of significant figures for the estimate.
#' @return A tibble with columns `Percentile` and `Xh` arranged by
#'   descending percentile.
#' @export
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   Concentration = rlnorm(200, 1, 1),
#'   Censored = rbinom(200, 1, 0.2),
#'   Site = sample(letters[1:3], 200, replace = TRUE)
#' )
#' Surv_quantile(dat)
Surv_quantile <- function(censored_data,
                          percentiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
                          type = "interp",
                          sig.fig = 3) {
  type <- rlang::arg_match(type, c("interp", "PiR", "nearest"))

  cenfrac <- mean(as.logical(censored_data[[2]]))

  weighted_km <- Surv_weighted(censored_data) |>
    dplyr::select("Concentration", "S")

  if (any(cenfrac > percentiles)) {
    cli::cli_warn(c(
      "!" = "The fraction of censored data is larger than one or more desired percentiles.",
      "i" = "This may produce {.val NA} values or unreliable estimates."
    ))
  }

  if (any(weighted_km$S < min(percentiles))) {
    cli::cli_warn(
      "Minimum desired percentile is below the minimum survival value; may produce {.val NA} values."
    )
  }

  fl_h <- function(percentile, S) {
    h_temp <- which.min(abs(S - percentile))
    if (type != "nearest" && S[h_temp] > percentile) h_temp <- h_temp + 1L
    h_temp
  }

  h_low  <- purrr::map_int(percentiles, fl_h, S = weighted_km$S)
  h_high <- h_low - 1L

  perc_df <- tibble::tibble(Percentile = percentiles)

  perc_df$Xh <- switch(
    type,
    interp = stats::approx(
      x = weighted_km$S[c(h_low, h_high)],
      y = weighted_km$Concentration[c(h_low, h_high)],
      xout = percentiles
    )$y,
    PiR     = weighted_km$Concentration[h_low],
    nearest = weighted_km$Concentration[h_low]
  )

  perc_df |>
    dplyr::arrange(dplyr::desc(.data$Percentile)) |>
    dplyr::mutate(Xh = signif(.data$Xh, sig.fig))
}
