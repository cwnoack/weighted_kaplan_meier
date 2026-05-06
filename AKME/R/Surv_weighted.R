#' Adjusted (weighted) Kaplan-Meier estimator of the survival function
#'
#' Calculates the adjusted Kaplan-Meier estimator (AKME) of the survival
#' function for right-censored concentration data, returning the estimator
#' `S` along with the weighted "at risk" (`Yw`) and "event" (`dw`) values
#' at each observed (uncensored) concentration. Follows the methodology of
#' Xie and Liu (2005).
#'
#' @param censored_data A data frame whose first three columns are
#'   (1) the measured concentration, (2) a censoring/detection flag where
#'   `1` indicates a non-detect, and (3) a unique site identifier.
#' @return A tibble with columns `Concentration`, `Site`, `weight`, `Yw`,
#'   `dw`, `P`, and `S` (the survival estimator).
#' @seealso [Surv_wconf()] for confidence bands, [Surv_quantile()] for
#'   quantile estimates, [group_km()] for per-group AKME.
#' @export
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   Concentration = rlnorm(50, 1, 1),
#'   Censored = rbinom(50, 1, 0.2),
#'   Site = sample(letters[1:3], 50, replace = TRUE)
#' )
#' Surv_weighted(dat)
Surv_weighted <- function(censored_data) {
  censored_data <- censored_data |>
    dplyr::rename(Concentration = 1, Censored = 2, Site = 3)
  check_input(censored_data)

  if (!is.factor(censored_data$Site)) {
    censored_data$Site <- factor(censored_data$Site)
  }

  site_weights <- calc_weights(censored_data$Site)

  data_mod <- censored_data |>
    dplyr::left_join(site_weights, by = "Site") |>
    dplyr::arrange(dplyr::desc(.data$Concentration)) |>
    dplyr::mutate(
      Yw = sum(.data$weight) - cumsum(.data$weight) + .data$weight
    )

  observed <- data_mod |>
    dplyr::filter(.data$Censored == 0) |>
    dplyr::select(-"Censored")

  obs_weight_tab <- observed |>
    dplyr::group_by(.data$Concentration) |>
    dplyr::summarise(dw = sum(.data$weight), .groups = "drop")

  observed |>
    dplyr::left_join(obs_weight_tab, by = "Concentration") |>
    dplyr::mutate(P = 1 - .data$dw / .data$Yw) |>
    dplyr::filter(!duplicated(.data$Concentration)) |>
    dplyr::mutate(
      S = cumprod(.data$P),
      S = pmax(.data$S, 0)
    ) |>
    tibble::as_tibble()
}
