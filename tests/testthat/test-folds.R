context("test-folds")


# Basic -------------------------------------------------------------------
test_that("folds balanced", {
  folds <- fold(n_folds = 10, n_obs = 50)
  
  expect_equal(tabulate(folds), rep(5, 10))
})

test_that("balks if n_obs isn't a scalar integer", {
  expect_error(fold(2, n_obs = "a"))
  expect_error(fold(2, n_obs = c(1,1)))
})

test_that("balks if n_obs isn't a scalar integer", {
  expect_error(fold(n_folds = "a", n_obs = 10))
  expect_error(fold(n_folds = c(1,1), n_obs = 10))
})

test_that("balks if fold weights aren't numeric", {
  expect_error(fold(n_folds = 1, n_obs = 1, fold_weights = "a"))
  expect_error(fold(n_folds = 1, n_obs = 1, fold_weights = factor(c(0.5, 0.5))))
})

test_that("balks if bounds are violated", {
  expect_error(fold(n_folds = -1, n_obs = 1, fold_weights = 1))
  expect_error(fold(n_folds = 2, n_obs = -1, fold_weights = 1))
  expect_error(fold(n_folds = 10, n_obs = 1, fold_weights = 1))
  expect_error(fold(n_folds = 1, n_obs = 1, fold_weights = -1))
})

test_that("balks if fold_weights is too short", {
  expect_error(fold(n_folds = 2, n_obs = 2, fold_weights = 1))
})

# Grouped -----------------------------------------------------------------
test_that("n_folds must be greater than n_obs", {
  expect_error(fold_grouped(n_folds = 10, groups = 1))
})

test_that("grouping preserved", {
  blk <- rep(1:2, each = 10)
  folds <- fold_grouped(n_folds = 2, groups = blk)
  
  expect_equal(folds[1:10], rep(folds[1], 10))
  expect_equal(folds[11:20], rep(folds[11], 10))
  expect_false(folds[1] == folds[11])
})


# Stratified --------------------------------------------------------------

test_that("balks if too many folds for strata", {
  expect_error(fold_stratified(n_folds = 2, strata = 1))
})

test_that("strata respected", {
  rat   <- rep(1:2, each = 10)
  folds <- fold_stratified(n_folds = 10, strata = rat)
  
  expect_equal(as.integer(table(folds)), rep(2, 10))
  
  fold_list <- split(seq_along(folds), folds)
  expect_equal(vapply(fold_list, function(x) {sum(x<11)}, 1L, USE.NAMES = FALSE),
               rep(1, 10))
})


# Grouped and Stratified --------------------------------------------------

test_that("balks if .f isn't a function", {
  
  fold_weights <- c(0.4, 0.6)
  strata <- rep(0:1, each = 50)
  groups <- rep(1:20, each = 5)
  
  expect_error(fold_grouped_stratified(n_folds = 2,
                                       fold_weights = fold_weights,
                                       groups = groups,
                                       strata = strata,
                                       .f = 1,
                                       i  = 1))
})

test_that("balks if argument lengths mismatch", {
  
  fold_weights <- c(0.4, 0.6)
  strata <- rep(0:1, each = 50)
  groups <- rep(1:20, each = 5)
  
  expect_error(fold_grouped_stratified(n_folds = 2,
                                       fold_weights = fold_weights,
                                       groups = groups,
                                       strata = 1,
                                       .f = `[`,
                                       i  = 1))
  
  expect_error(fold_grouped_stratified(n_folds = 2,
                                       fold_weights = fold_weights,
                                       groups = 1,
                                       strata = 1,
                                       .f = `[`,
                                       i  = 1))
  
})

test_that("groups and strata respected", {
  
  fold_weights <- c(0.4, 0.6)
  strata <- rep(0:1, each = 50)
  groups <- rep(1:20, each = 5)
  
  folds <- fold_grouped_stratified(n_folds = 2,
                                   fold_weights = fold_weights,
                                   groups = groups,
                                   strata = strata,
                                   .f = `[`,
                                   i  = 1)
  
  folds_per_group <- tapply(folds, groups, function(x) length(unique(x)))
  
  # Should only be two folds
  expect_true(all(folds %in% 1:2))
  
  # Folds should be split by weights
  expect_equal(as.numeric(table(folds)) / length(folds), fold_weights)
  
  # Fold assignment should happen at cluster level
  # So no cluster should appear in multiple folds
  expect_equal(as.numeric(folds_per_group), rep(1, length(folds_per_group)))
  
  # Both strata should be in each fold
  for (fold in unique(folds)) {
    expect_true(any(strata[folds == fold] == 0))
    expect_true(any(strata[folds == fold] == 1))
  }
  
})

