#' @rdname analyze
#' @export
#' @import learners
#' 
analyze_residuals <- function(features, tmt, learners,
                              task_type = "binary",
                              weight = NULL, y = NULL, block = NULL,
                              cluster = NULL,
                              loss = list(rmse = loss_funs$rmse),
                              test_method = "holdout",
                              test_split = ifelse(test_method == "holdout", 0.3, 5L),
                              tune_split = 5L,
                              permutations = 100L, parallel = NULL,
                              random_seed = 1, verbose = FALSE) {
  
  start_time <- Sys.time()
  set.seed(random_seed, kind = "L'Ecuyer-CMRG")
  

  # Dev Flags ---------------------------------------------------------------
  if (test_method == "cv") rlang::abort("cv not implemented")
  
  
  # Defensive Checks --------------------------------------------------------
  say(verbose, "Checking inputs...")
  task_type <- check_task_type(task_type)
  
  features <- check_features(features)
  tmt      <- check_tmt(tmt, task_type)
  weight   <- check_weight(weight)
  y        <- check_y(y)
  
  block    <- check_block(block)
  cluster  <- check_cluster(cluster)
  learners <- purrr::map(learners, check_learner)
  # loss     <- check_loss(loss, task_type)
  
  test_method  <- check_method(test_method)
  tune_split   <- check_tune_split(tune_split)
  test_split   <- check_test_split(test_split, test_method)
  permutations <- check_permutations(permutations, task_type)
  parallel     <- check_parallel(parallel)
  
  check_block_cluster(block, cluster)
  
  if (length(features) != 2) rlang::abort("features must be al list of 2 matrices")
  
  # Preprocess Arguments ----------------------------------------------------
  feat_tmt_match <- purrr::map_lgl(features, ~nrow(.x) == length(tmt))
  if (any(feat_tmt_match == FALSE)) { 
    rlang::abort("nrow(features) and length(tmt) must be the same")
  }
  
  names(features) <- names(features) %||% 
    paste0("feature_set_", seq_along(features))
  
  if (is.factor(tmt)) tmt <- as.integer(tmt) - 1L
  
  # Folds -------------------------------------------------------------------
  say(verbose, "Setting up testing and tuning folds...")
  folds <- make_folds(test_method = test_method,
                      tune_split  = tune_split,
                      test_split  = test_split,
                      task_type   = task_type,
                      tmt         = tmt,
                      block       = block,
                      cluster     = cluster)
  
  feat_test_match <- purrr::map_lgl(features, ~nrow(.x) == length(folds$test))
  if (any(feat_test_match == FALSE)) { 
    rlang::abort("nrow(features) and length(folds$test) must be the same")
  }
  
  feat_tune_match <- purrr::map_lgl(features, ~nrow(.x) == length(folds$tune))
  if (any(feat_tune_match == FALSE)) { 
    rlang::abort("nrow(features) and length(folds$tune) must be the same")
  }
  
  fit_mask <- switch(test_method,
                     holdout = which(folds$test == 1),
                     cv = rep(TRUE, length(folds$tune)))
  
  test_mask <- switch(test_method,
                      holdout = which(folds$test == 2),
                      cv = rep(TRUE, length(folds$test)))
  
  # Permutations ------------------------------------------------------------
  if (rlang::is_null(permutations) | rlang::is_list(permutations)) { 
    permuted_tmt <- permutations
  }
  
  if (rlang::is_scalar_integerish(permutations)) {
    
    say(verbose, "Permuting treatment assignment...")
    permuted_tmt <- permute_tmt(tmt[test_mask],
                                n_perm  = permutations,
                                block   = block[test_mask],
                                cluster = cluster[test_mask])
  }
  
  # OOS Train Set Predictions -----------------------------------------------
  predictions_1 <- 
    tune_predict_oos(object     = learners[[1]],
                     features   = features[[1]][fit_mask, , drop = FALSE],
                     tgt        = tmt[fit_mask],
                     weight     = weight[fit_mask],
                     tune_folds = folds$tune[fit_mask])
  
  residuals_1 <- tmt[fit_mask] - predictions_1
  
  # Tune --------------------------------------------------------------------
  tuned_learners <- list(
    # Tune first learner on treatment
    tune(object     = learners[[1]],
         features   = features[[1]][fit_mask, , drop = FALSE],
         tgt        = tmt[fit_mask],
         wt         = weight[fit_mask],
         tune_folds = folds$tune[fit_mask]),
    # Tune second learner on residuals
    tune(object     = learners[[2]],
         features   = features[[2]][fit_mask, , drop = FALSE],
         tgt        = residuals_1,
         wt         = weight[fit_mask],
         tune_folds = folds$tune[fit_mask])
  )
  
  # Train -------------------------------------------------------------------
  trained_learners <- list(
    # Train first learner on treatment
    train(object   = tuned_learners[[1]],
          features = features[[1]][fit_mask, , drop = FALSE],
          tgt      = tmt[fit_mask],
          wt       = weight[fit_mask]),
    # Train second learner on residuals
    train(object   = tuned_learners[[2]],
          features = features[[2]][fit_mask, , drop = FALSE],
          tgt      = residuals_1,
          wt       = weight[fit_mask])
  )
  

  # Predict -----------------------------------------------------------------
  predictions <- purrr::map2(trained_learners, features, stats::predict)
  
  residuals   <- list(tmt - predictions[[1]],
                      tmt - predictions[[1]] - predictions[[2]])

  # Perm Test ---------------------------------------------------------------
  perm_test_result <- perm_test(pred = predictions[[2]][test_mask],
                                permuted_pred = NULL,
                                tmt = tmt[test_mask],
                                permuted_tmt = permuted_tmt,
                                weight  = weight[test_mask],
                                loss = loss)

  # Return ------------------------------------------------------------------
  list(learners = trained_learners,
       predictions = predictions,
       perm_test = perm_test_result)
}

#' @rdname analyze
#' @export
analyse_residuals <- analyze_residuals