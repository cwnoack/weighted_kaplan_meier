test_that("Surv_weighted matches a hand-computed example (single site, no censoring)", {
  dat <- data.frame(
    Concentration = c(10, 20, 30, 40),
    Censored = c(0L, 0L, 0L, 0L),
    Site = rep("A", 4)
  )
  out <- Surv_weighted(dat)

  # Sorted descending by Concentration: 40, 30, 20, 10
  # All weights = 1/4 = 0.25; sum(w) = 1
  # Yw = sum(w) - cumsum(w) + w  =>  1.00, 0.75, 0.50, 0.25
  # dw = 0.25; P = 1 - dw/Yw    =>  0.75, 2/3, 0.5, 0
  # S = cumprod(P)               =>  0.75, 0.50, 0.25, 0
  expect_equal(out$Concentration, c(40, 30, 20, 10))
  expect_equal(out$weight, rep(0.25, 4))
  expect_equal(out$Yw, c(1.00, 0.75, 0.50, 0.25))
  expect_equal(out$dw, rep(0.25, 4))
  expect_equal(out$S, c(0.75, 0.50, 0.25, 0))
})

test_that("Surv_weighted's S at the kth-largest concentration equals (N - k) / N for uniform-weight, untied, uncensored data", {
  withr::local_seed(42)
  N <- 30
  conc <- sort(stats::rlnorm(N, 1, 1)) + seq(0, 1e-6, length.out = N)
  dat <- data.frame(
    Concentration = conc, Censored = rep(0L, N), Site = rep("A", N)
  )
  out <- Surv_weighted(dat)

  expected <- (N - seq_len(N)) / N
  expect_equal(out$S, expected, tolerance = 1e-12)
})

test_that("Surv_weighted returns a tibble", {
  dat <- data.frame(
    Concentration = c(1.5, 2.5, 3.5),
    Censored = c(0L, 1L, 0L),
    Site = c("a", "b", "a")
  )
  out <- Surv_weighted(dat)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("Concentration", "Site", "weight", "Yw", "dw", "P", "S") %in% names(out)))
})

test_that("Surv_weighted survival values are bounded in [0, 1]", {
  withr::local_seed(7)
  dat <- make_dummy_data(seed = 7, N = 80) |>
    dplyr::filter(.data$Dataset == "Control") |>
    dplyr::select(-"Dataset")
  out <- Surv_weighted(dat)
  expect_true(all(out$S >= 0 & out$S <= 1))
})
