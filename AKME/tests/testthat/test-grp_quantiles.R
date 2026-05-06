test_that("grp_quantiles pivots one column per Dataset, descending Percentile", {
  withr::local_seed(7)
  dat <- make_dummy_data(seed = 7, N = 60)
  q <- grp_quantiles(dat, percentiles = c(0.25, 0.5, 0.75))

  expect_s3_class(q, "tbl_df")
  expect_equal(nrow(q), 3L)
  expect_true("Percentile" %in% names(q))
  expect_setequal(setdiff(names(q), "Percentile"),
                  c("Control", "High", "Highest"))
  expect_equal(q$Percentile, c(0.75, 0.5, 0.25))
})

test_that("group_km returns AKME columns plus Dataset", {
  withr::local_seed(8)
  dat <- make_dummy_data(seed = 8, N = 40)
  out <- group_km(dat)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("Dataset", "Concentration", "S") %in% names(out)))
  expect_setequal(unique(as.character(out$Dataset)),
                  c("Control", "High", "Highest"))
})
