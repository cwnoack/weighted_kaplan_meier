test_that("Surv_quantile shape and ordering", {
  withr::local_seed(1)
  dat <- make_dummy_data(seed = 1, N = 60) |>
    dplyr::filter(.data$Dataset == "High") |>
    dplyr::select(-"Dataset")
  q <- Surv_quantile(dat, percentiles = c(0.1, 0.25, 0.5, 0.75, 0.9))

  expect_s3_class(q, "tbl_df")
  expect_named(q, c("Percentile", "Xh"))
  expect_equal(q$Percentile, sort(q$Percentile, decreasing = TRUE))
})

test_that("Surv_quantile rejects unknown `type`", {
  dat <- data.frame(
    Concentration = c(1, 2, 3, 4, 5),
    Censored = rep(0L, 5),
    Site = rep("A", 5)
  )
  expect_error(Surv_quantile(dat, type = "bogus"))
})

test_that("Surv_quantile honors sig.fig", {
  withr::local_seed(99)
  dat <- make_dummy_data(seed = 99, N = 100) |>
    dplyr::filter(.data$Dataset == "Control") |>
    dplyr::select(-"Dataset")
  q <- Surv_quantile(dat, percentiles = 0.5, sig.fig = 2)
  expect_equal(q$Xh, signif(q$Xh, 2))
})
