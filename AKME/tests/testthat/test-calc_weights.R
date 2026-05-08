test_that("calc_weights returns 1/n_k per site", {
  sites <- c("a", "a", "b", "b", "b", "c")
  res <- calc_weights(sites)

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("Site", "weight"))
  expect_setequal(as.character(res$Site), c("a", "b", "c"))

  ord <- order(as.character(res$Site))
  expect_equal(res$weight[ord], c(1 / 2, 1 / 3, 1))
})
