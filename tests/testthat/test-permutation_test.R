context("test-permutation_test")

test_that("NULL in NULL out", {
  expect_null(perm_test(1, 1, permuted_tmt = NULL))
})

test_that("balks if predictions aren't vector or matrix", {
  expect_error(perm_test(pred = data.frame(1),
                         tmt = 1,
                         permuted_tmt = list(1)),
               class = "error_s3_class")
})

test_that("basic inputs", {
  expect_error(perm_test(pred = matrix(1),
                         tmt = 1,
                         permuted_tmt = list(1)),
               NA)
  
  expect_error(perm_test(pred = matrix(1),
                         tmt = 1,
                         permuted_tmt = list(1),
                         loss = loss_funs$rmse),
               NA)
  
  expect_error(perm_test(pred = matrix(1),
                         tmt = 1,
                         permuted_tmt = list(1),
                         loss = list(loss_funs$rmse, loss_funs$rmse)),
               NA)
})
