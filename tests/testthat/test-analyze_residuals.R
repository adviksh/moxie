context("test-analyze_residuals")

test_that("runs with some arguments", {
  expect_error(analyze_residuals(features = list(toy_data$x[,1:2],
                                                 toy_data$x[,1:5]),
                                 tmt = toy_data$tmt,
                                 learners = list(mean_learner,
                                                 mean_learner),
                                 loss = list(rmse = loss_funs$rmse,
                                             neg_cov = loss_funs$neg_cov),
                                 permutations = 10),
               NA)
  
})
