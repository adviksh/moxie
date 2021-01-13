toy_data <- list(
  x       = matrix(rnorm(24 * 5), nrow = 24),
  block   = rep(1:4, each = 6),
  cluster = rep(1:12, each = 2),
  tmt     = factor(rep(c(0,0,1,1), length.out = 24)),
  y       = matrix(rnorm(24 * 5), nrow = 24),
  train_folds = rep(1:2, each = 12),
  tune_folds  = c(rep(1:2, each = 6),
                  rep(NA, 12)))
