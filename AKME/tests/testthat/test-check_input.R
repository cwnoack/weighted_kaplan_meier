test_that("check_input passes a well-formed three-column frame", {
  good <- data.frame(
    Concentration = c(1.2, 3.4, 5.6),
    Censored = c(0L, 1L, 0L),
    Site = c("a", "b", "a")
  )
  expect_invisible(check_input(good))
  expect_identical(check_input(good), good)
})

test_that("check_input rejects non-data-frame input", {
  expect_error(check_input(list(1, 2, 3)), class = "rlang_error")
  expect_error(check_input(matrix(1, 3, 3)), class = "rlang_error")
})

test_that("check_input rejects too-few columns", {
  expect_error(check_input(data.frame(x = 1, y = 2)), "at least three columns")
})

test_that("check_input rejects non-numeric concentration column", {
  bad <- data.frame(
    Concentration = c("a", "b", "c"),
    Censored = c(0L, 0L, 0L),
    Site = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  expect_error(check_input(bad), "concentration")
})

test_that("check_input rejects bad censoring-flag class", {
  bad <- data.frame(
    Concentration = c(1, 2, 3),
    Censored = list("not", "a", "vector"),
    Site = c("a", "b", "c")
  )
  expect_error(check_input(bad))
})
