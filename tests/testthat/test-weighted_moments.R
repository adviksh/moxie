context("test-weighted_moments")


# weighted_var/sd ---------------------------------------------------------
test_that("weighted_var/sd computation", {
  x <- c(2,2,4,5,5,5)
  expect_equal(var(x), weighted_var(unique(x), table(x)))
  expect_equal(sd(x), weighted_sd(unique(x), table(x)))
})

# weighted_var/sd -----------------------------------------------------
test_that("weighted_var/sd NA handling", {
  x <- c(2,2,4,5,5,5, NA)
  
  x_unique <- unique(x)
  w <- as.numeric(table(x, useNA = 'always'))
  
  expect_equivalent(var(x, na.rm = TRUE), weighted_var(x_unique, w, na.rm = TRUE))
  expect_equivalent(sd(x, na.rm = TRUE), weighted_sd(x_unique, w, na.rm = TRUE))
  
  expect_identical(NA_real_, weighted_var(x_unique, w, na.rm = FALSE))
  expect_identical(NA_real_, weighted_sd(x_unique, w, na.rm = FALSE))
})