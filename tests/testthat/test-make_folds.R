context("test-make_folds")

# Holdout Method ----------------------------------------------------------

test_that("no overlap between train and holdout", {
  folds_binary <- make_folds(test_method = "holdout",
                             task_type = "binary",
                             test_split = 0.5,
                             tune_split = 2,
                             tmt = toy_data$tmt)
  
  folds_regr <- make_folds(test_method = "holdout",
                           task_type = "regression",
                           test_split = 0.5,
                           tune_split = 2,
                           tmt = as.integer(toy_data$tmt))
  
  expect_equal(length(folds_binary$test), length(toy_data$tmt))
  expect_equal(length(folds_binary$tune), length(toy_data$tmt))
  
  expect_identical(which(folds_binary$test == 2),
                   which(is.na(folds_binary$tune)))
  
  expect_equal(length(folds_regr$test), length(toy_data$tmt))
  expect_equal(length(folds_regr$tune), length(toy_data$tmt))
  
  expect_identical(which(folds_regr$test == 2),
                   which(is.na(folds_regr$tune)))
})

test_that("holdout method respects blocks", {
  folds <- make_folds(test_method = "holdout",
                      task_type = "binary",
                      test_split = 0.5,
                      tune_split = 2,
                      tmt = toy_data$tmt,
                      block = toy_data$block)
  
  train_blocking <- tapply(folds$test, toy_data$block,
                           function(x) {length(unique(x))})
  
  tune_blocking <- tapply(folds$tune, toy_data$block,
                          function(x) {length(unique(x))})
  
  expect_equal(length(folds$test), length(toy_data$tmt))
  expect_equal(length(folds$tune), length(toy_data$tmt))
  
  expect_equal(which(folds$test == 2), which(is.na(folds$tune)))
  
  expect_equal(as.integer(train_blocking), rep(1L, length(train_blocking)))
  expect_equal(as.integer(tune_blocking), rep(1L, length(tune_blocking)))
})

test_that("holdout method respects clusters", {
  folds <- make_folds(test_method = "holdout",
                      task_type = "binary",
                      test_split = 0.5,
                      tune_split = 2,
                      tmt = toy_data$tmt,
                      cluster = toy_data$cluster)
  
  train_clustering <- tapply(folds$test, toy_data$cluster,
                             function(x) {length(unique(x))})
  
  tune_clustering <- tapply(folds$tune, toy_data$cluster,
                            function(x) {length(unique(x))})
  
  expect_equal(length(folds$test), length(toy_data$tmt))
  expect_equal(length(folds$tune), length(toy_data$tmt))
  
  expect_equal(which(folds$test == 2), which(is.na(folds$tune)))
  
  expect_equal(as.integer(train_clustering), rep(1L, length(train_clustering)))
  expect_equal(as.integer(tune_clustering), rep(1L, length(tune_clustering)))
})

test_that("holdout method respects blocks and clusters", {
  folds <- make_folds(test_method = "holdout",
                      task_type = "binary",
                      test_split = 0.5,
                      tune_split = 2,
                      tmt = toy_data$tmt,
                      block = toy_data$block,
                      cluster = toy_data$cluster)
  
  train_blocking <- tapply(folds$test, toy_data$block,
                           function(x) {length(unique(x))})
  
  tune_blocking <- tapply(folds$tune, toy_data$block,
                          function(x) {length(unique(x))})
  
  expect_equal(length(folds$test), length(toy_data$tmt))
  expect_equal(length(folds$tune), length(toy_data$tmt))
  
  expect_equal(which(folds$test == 2), which(is.na(folds$tune)))
  
  expect_equal(as.integer(train_blocking), rep(1L, length(train_blocking)))
  expect_equal(as.integer(tune_blocking), rep(1L, length(tune_blocking)))
})

# CV Method ---------------------------------------------------------------

test_that("cv method respects blocks", {
  folds <- make_folds(test_method = "cv",
                      task_type = "binary",
                      tune_split = 4,
                      tmt = toy_data$tmt,
                      block = toy_data$block)
  
  train_blocking <- tapply(folds$tune, toy_data$block,
                           function(x) {length(unique(x))})
  
  tune_blocking <- tapply(folds$tune, toy_data$block,
                          function(x) {length(unique(x))})
  
  expect_equal(length(folds$test), length(toy_data$tmt))
  expect_equal(length(folds$tune), length(toy_data$tmt))
  
  expect_equal(as.integer(train_blocking), rep(1L, length(train_blocking)))
  expect_equal(as.integer(tune_blocking), rep(1L, length(tune_blocking)))
})

test_that("cv method respects clusters", {
  folds <- make_folds(test_method = "cv",
                      task_type = "binary",
                      tune_split = 4,
                      tmt = toy_data$tmt,
                      cluster = toy_data$cluster)
  
  train_clustering <- tapply(folds$test, toy_data$cluster,
                             function(x) {length(unique(x))})
  
  tune_clustering <- tapply(folds$tune, toy_data$cluster,
                            function(x) {length(unique(x))})
  
  expect_equal(length(folds$test), length(toy_data$tmt))
  expect_equal(length(folds$tune), length(toy_data$tmt))
  
  expect_equal(as.integer(train_clustering), rep(1L, length(train_clustering)))
  expect_equal(as.integer(tune_clustering), rep(1L, length(tune_clustering)))
})

test_that("cv method respects blocks and clusters", {
  folds <- make_folds(test_method = "cv",
                      task_type = "binary",
                      tune_split = 4,
                      tmt = toy_data$tmt,
                      block = toy_data$block,
                      cluster = toy_data$cluster)
  
  train_blocking <- tapply(folds$test, toy_data$block,
                           function(x) {length(unique(x))})
  
  tune_blocking <- tapply(folds$tune, toy_data$block,
                          function(x) {length(unique(x))})
  
  expect_equal(length(folds$test), length(toy_data$tmt))
  expect_equal(length(folds$tune), length(toy_data$tmt))
  
  expect_equal(as.integer(train_blocking), rep(1L, length(train_blocking)))
  expect_equal(as.integer(tune_blocking), rep(1L, length(tune_blocking)))
})