context("test-analyze_features")

for (tm in c("in_sample", "holdout", "cv")) {
  
  test_that("runs with default arguments, one feature set", {
    expect_error(analyze_features(features     = toy_data$x, 
                                  tmt          = toy_data$tmt,
                                  learner      = mean_learner,
                                  test_method  = tm,
                                  permutations = 10L),
                 NA)
  })
  
  test_that("runs with default arguments, many feature sets", {
    expect_error(analyze_features(features     = list(toy_data$x, toy_data$x),
                                  tmt          = toy_data$tmt,
                                  learner      = mean_learner,
                                  test_method  = tm,
                                  permutations = 10L),
                 NA)
    
  })
  
  test_that("runs with blocking", {
    expect_error(analyze_features(features     = toy_data$x, 
                                  tmt          = toy_data$tmt,
                                  learner      = mean_learner,
                                  y            = toy_data$y,
                                  block        = toy_data$block,
                                  test_split   = 0.3,
                                  tune_split   = 2,
                                  test_method  = tm,
                                  permutations = 10L),
                 NA)
  })
  
  test_that("runs with clusters", {
    expect_error(analyze_features(features     = toy_data$x, 
                                  tmt          = toy_data$tmt,
                                  learner      = mean_learner,
                                  y            = toy_data$y,
                                  cluster      = toy_data$cluster,
                                  test_split   = 0.3,
                                  tune_split   = 2,
                                  test_method  = tm,
                                  permutations = 6L),
                 NA)
  })
  
  test_that("runs with blocking and clusters", {
    expect_error(analyze_features(features     = toy_data$x, 
                                  tmt          = toy_data$tmt,
                                  learner      = mean_learner,
                                  y            = toy_data$y,
                                  block        = toy_data$block,
                                  cluster      = toy_data$cluster,
                                  test_split   = 0.3,
                                  tune_split   = 2,
                                  test_method  = tm,
                                  permutations = 3L),
                 NA)
  })
}