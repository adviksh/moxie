context("test-analyze_feature_set")

test_that("all `test_method`s", {
  
  for (tm in c("in_sample", "cv", "holdout")) {
    expect_error(analyze_feature_set(feature_set = toy_data$x,
                                     feature_set_name = "x",
                                     tmt     = toy_data$tmt,
                                     learner = mean_learner,
                                     task_type = "binary",
                                     weight    = rep(1, nrow(toy_data$x)),
                                     y         = toy_data$y,
                                     loss      = loss_funs$rmse,
                                     test_method = tm,
                                     folds = list(tune = toy_data$tune_folds,
                                                  test = toy_data$train_folds),
                                     permuted_tmt = NULL,
                                     parallel = NULL,
                                     verbose  = FALSE),
                 NA)
    
    permuted_tmt <- list(toy_data$tmt[toy_data$train_folds == 2],
                         sample(toy_data$tmt[toy_data$train_folds == 2]))
    
    expect_error(analyze_feature_set(feature_set = toy_data$x,
                                     feature_set_name = "x",
                                     tmt       = toy_data$tmt,
                                     learner   = mean_learner,
                                     task_type = "binary",
                                     weight    = rep(1, nrow(toy_data$x)),
                                     y         = toy_data$y,
                                     loss      = loss_funs$rmse,
                                     test_method = tm,
                                     folds = list(tune = toy_data$tune_folds,
                                                  test = toy_data$train_folds),
                                     permuted_tmt = permuted_tmt,
                                     parallel = NULL,
                                     verbose = FALSE),
                 NA)
  }
  
})
