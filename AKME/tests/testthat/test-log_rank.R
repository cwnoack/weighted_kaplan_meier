test_that("log_rank output structure", {
  withr::local_seed(5)
  dat <- make_dummy_data(seed = 5, N = 40)
  pair <- dplyr::filter(dat, .data$Dataset != "High")
  out <- withr::with_seed(1,
    log_rank(pair, comp_group = "Highest", rho = 1,
             method = "perm", boots = 50, alternative = "two.sided")
  )
  expect_named(out, c("G_test", "boot_G", "p.val", "rho"))
  expect_length(out$boot_G, 50L)
  expect_true(out$p.val >= 0 && out$p.val <= 1)
  expect_equal(out$rho, 1)
})

test_that("log_rank flags strongly separated groups (small p)", {
  withr::local_seed(2)
  dat <- make_dummy_data(seed = 2, N = 80, means = c(0, 5))
  out <- withr::with_seed(1,
    log_rank(dat, rho = 0, method = "perm", boots = 200,
             alternative = "two.sided")
  )
  expect_lt(out$p.val, 0.05)
})

test_that("log_rank rejects unknown method/alternative", {
  withr::local_seed(3)
  dat <- make_dummy_data(seed = 3, N = 30, means = c(1, 2))
  expect_error(log_rank(dat, method = "bogus"))
  expect_error(log_rank(dat, alternative = "wat"))
})
