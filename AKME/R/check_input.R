#' Check inputs to Kaplan-Meier estimator functions
#'
#' Validates that `censored_data` is a three-column data frame whose columns
#' hold (1) the measured concentration, (2) a censoring/detection flag, and
#' (3) a site identifier, in that order.
#'
#' @param censored_data A data frame of (at least) three columns.
#' @return `censored_data`, returned invisibly. Called for its side effect of
#'   raising an informative error when the input is malformed.
#' @export
#' @examples
#' good <- data.frame(
#'   Concentration = rlnorm(100, 1, 2),
#'   Censored = rbinom(100, 1, 0.2),
#'   Site = sample(letters[1:3], 100, replace = TRUE)
#' )
#' check_input(good)
check_input <- function(censored_data) {
  if (!is.data.frame(censored_data)) {
    cli::cli_abort(
      "{.arg censored_data} must be a data frame, not {.cls {class(censored_data)[1]}}."
    )
  }
  if (ncol(censored_data) < 3L) {
    cli::cli_abort(
      "{.arg censored_data} must have at least three columns; got {ncol(censored_data)}."
    )
  }
  if (!is.numeric(censored_data[[1]])) {
    cli::cli_abort(c(
      "Column 1 (concentration) must be {.cls numeric}.",
      "x" = "Got {.cls {class(censored_data[[1]])[1]}}."
    ))
  }
  if (!inherits(censored_data[[2]], c("numeric", "factor", "logical", "integer"))) {
    cli::cli_abort(c(
      "Column 2 (censoring flag) must be numeric, factor, logical, or integer.",
      "x" = "Got {.cls {class(censored_data[[2]])[1]}}."
    ))
  }
  if (!inherits(censored_data[[3]], c("factor", "character", "integer", "numeric"))) {
    cli::cli_abort(c(
      "Column 3 (site identifier) must be factor, character, integer, or numeric.",
      "x" = "Got {.cls {class(censored_data[[3]])[1]}}."
    ))
  }
  invisible(censored_data)
}
