make_dummy_data <- function(seed = 8675309, N = 100,
                            means = c(1, 1.5, 2.5),
                            group_names = NULL) {
  withr::local_seed(seed)
  if (is.null(group_names)) {
    defaults <- c("Control", "High", "Highest", "G4", "G5", "G6")
    group_names <- defaults[seq_along(means)]
  }
  stopifnot(length(group_names) == length(means))

  R <- data.frame(vapply(means, function(mu) stats::rlnorm(N, mu, 1),
                         numeric(N)))
  colnames(R) <- group_names

  long <- tidyr::pivot_longer(
    tibble::as_tibble(R),
    cols = dplyr::all_of(group_names),
    names_to = "Dataset",
    values_to = "Concentration"
  )
  long |>
    dplyr::mutate(
      DL = sample(c(1, 10), size = dplyr::n(),
                  replace = TRUE, prob = c(0.6, 0.4)),
      Censored = as.integer(.data$Concentration < .data$DL),
      Concentration = dplyr::if_else(.data$Censored == 1L,
                                     .data$DL, .data$Concentration),
      Site = sample(LETTERS[1:10], size = dplyr::n(),
                    replace = TRUE, prob = 1:10 / sum(1:10))
    ) |>
    dplyr::select("Dataset", "Concentration", "Censored", "Site")
}
