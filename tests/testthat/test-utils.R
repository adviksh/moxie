context("test-utils")

test_that("NULL in NULL out", {
  expect_null(perm_p_val(1, NULL))
})

test_that("commas", {
  expect_identical(commas(c("a", "b")),
                   "a, b")
})