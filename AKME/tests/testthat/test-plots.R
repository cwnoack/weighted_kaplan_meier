test_that("plot_wKM returns a ggplot object", {
  withr::local_seed(4)
  dat <- make_dummy_data(seed = 4, N = 40)
  km <- group_km(dat)
  p <- plot_wKM(km, log_scale = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_wKM rejects non-positive concentrations on a log scale", {
  bad <- tibble::tibble(
    Concentration = c(0, 1, 2),
    S = c(1, 0.5, 0),
    Dataset = "x"
  )
  expect_error(plot_wKM(bad, log_scale = TRUE), "positive")
})

test_that("G_rho_hist returns a ggplot object", {
  fake <- list(
    G_test = 1.2,
    boot_G = stats::rnorm(100),
    p.val = 0.123,
    rho = 1
  )
  expect_s3_class(G_rho_hist(fake), "ggplot")
})
