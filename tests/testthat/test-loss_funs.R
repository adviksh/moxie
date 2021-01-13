context("test-loss")

test_that('mean squared error is calculated correctly', {
  
  expect_equal(mse(c(0,0,1,1), c(0,0,1,1)),
               mse(factor(c("a", "a", "b", "b")), c(0,0,1,1)))
  
  expect_equal(mse(c(0,0,1,1), c(0,0,1,1)), 0)
  expect_equal(mse(c(0,0,1,1), c(1,1,0,0)), 1)
  expect_equal(mse(c(0,0,2,2), c(2,2,0,0)), 4)
  
  # Weights
  expect_equal(mse(c(0,0,2,2), c(2,2,0,0)),
               mse(c(0,2), c(2,0), c(2,2)))
})

test_that("roc_area_above is calculated correctly", {
  
  expect_equal(roc_area_above(c(0,0,1,1), c(1, 1, 0.5, 0.5)), 1)
  expect_equal(roc_area_above(c(0,0,1,1), c(0, 1, 0.5, 0.5)), 0.5)
  expect_equal(roc_area_above(c(0,0,1,1), c(0, 0, 0.5, 0.5)), 0)
  
  expect_equal(roc_area_above(c(0,0,1,1), c(0.5, 0.5, 0.5, 0.5)),
               roc_area_above(c(0,0,1,1), c(0.6, 0.6, 0.6, 0.6)))
  
  # Weights
  expect_equal(roc_area_above(c(0,0,1,1), c(0, 1, 0.5, 0.5)),
               roc_area_above(c(0,0,1), c(0, 1, 0.5), c(1,1,2)))
  
  expect_equal(roc_area_above(c(0,0,1), c(0, 1, 0.5), c(1,1,2)),
               roc_area_above(c(0,0,1), c(0, 1, 0.5), c(1,1,3)))
  
  expect_gt(roc_area_above(c(0,1,0,1), c(0, 0.2, 0.4, 0.6), c(1,1,1,1)),
            roc_area_above(c(0,1,0,1), c(0, 0.2, 0.4, 0.6), c(2,1,1,1)))
  
})

test_that("neg_cov (negative covariance) is calculated correctly", {
  
  x_1 <- rep(0:1, each = 10)
  
  expect_equal(neg_cov(x_1, x_1), 
               -1 * cov(x_1, x_1))
  
  # weights
  expect_equal(neg_cov(x_1, x_1),
               neg_cov(0:1, 0:1, c(10, 10)))
  
})

test_that("neg_bern_loglik", {
  
  expect_gt(neg_bern_loglik(0, 0.25),
            neg_bern_loglik(1, 0.25))
  
  expect_equal(neg_bern_loglik(0, 0.25),
               neg_bern_loglik(1, 0.75))
  
  expect_equal(neg_bern_loglik(c(1,1), c(0.5, 0.5)),
               neg_bern_loglik(1, 0.5, 2))
  
  
})
