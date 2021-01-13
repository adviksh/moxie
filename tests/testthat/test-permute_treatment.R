context("test-permute_tmt")

test_that("NULL in, NULL out", {
  expect_null(permute_tmt(tmt = 1, n_perm = NULL))
})

test_that("Balks if tmt isn't clustered", {
  expect_error(permute_tmt(tmt = c("a", "b"),
                           cluster = c(1,1)))
})

test_that("Preserves number of treated and control", {
  treatment <- c(letters, letters)
  expect_true(all(table(permute_tmt(treatment)) == 2))
})

test_that("respects clusters", {
  cluster <- sample(1:5, 100, replace = TRUE)
  cluster_treatment <- letters[seq_along(unique(cluster))]
  treatment <- cluster_treatment[cluster]
  
  permuted_treatment <- permute_tmt(treatment, cluster = cluster)[[1]]
  
  unique_treat_per_cluster <- tapply(permuted_treatment, cluster, function(x) {length(unique(x))})
  perm_treat_levels <- unique(permuted_treatment)
  
  # Just one treatment per cluster
  expect_true(all(unique_treat_per_cluster == 1))
  
  # No treatments dropped
  expect_equal(levels(treatment), levels(permuted_treatment))
})

test_that("respects blocks", {
  block <- 1:26
  treatment <- letters[block]
  
  permuted_treatment <- permute_tmt(treatment, block = block)[[1]]
  
  # Just one unit per block, and treatment shouldn't move across blocks
  expect_equal(treatment, permuted_treatment)
  
})

test_that("respects blocks and clusters", {
  block <- rep(1:2, each = 50)
  cluster <- 10*block + rep(1:5, times = 20)
  treatment <- factor(letters[cluster])
  
  permuted_treatment <- permute_tmt(treatment, cluster = cluster, block = block)[[1]]
  
  unique_treat_per_cluster <- tapply(permuted_treatment, cluster, function(x) {length(unique(x))})
  perm_treat_levels <- unique(permuted_treatment)
  
  # Just one treatment per cluster
  expect_true(all(unique_treat_per_cluster == 1))
  
  # No treatments dropped
  expect_equal(unique(sort(treatment)), unique(sort(permuted_treatment)))
  
  # Treatment doesn't move across blocks
  expect_equal(sort(treatment[1:50]), sort(permuted_treatment[1:50]))
  expect_equal(sort(treatment[51:100]), sort(permuted_treatment[51:100]))
})
