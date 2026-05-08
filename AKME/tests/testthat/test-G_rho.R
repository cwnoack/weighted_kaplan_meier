test_that("G_rho is symmetric in its definition: G_rho(A, B, 0) == -G_rho(B, A, 0)", {
  withr::local_seed(11)
  dat <- make_dummy_data(seed = 11, N = 60)
  A <- dat |> dplyr::filter(.data$Dataset == "Control") |> dplyr::select(-"Dataset")
  B <- dat |> dplyr::filter(.data$Dataset == "High")    |> dplyr::select(-"Dataset")

  expect_equal(G_rho(A, B, rho = 0), -G_rho(B, A, rho = 0), tolerance = 1e-8)
})

test_that("G_rho returns a single finite numeric", {
  withr::local_seed(13)
  dat <- make_dummy_data(seed = 13, N = 50)
  A <- dat |> dplyr::filter(.data$Dataset == "Control") |> dplyr::select(-"Dataset")
  B <- dat |> dplyr::filter(.data$Dataset == "High")    |> dplyr::select(-"Dataset")

  G <- G_rho(A, B, rho = 1)
  expect_length(G, 1L)
  expect_true(is.finite(G))
})
