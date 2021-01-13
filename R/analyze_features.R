#' @rdname analyze
#' @export
#' @import learners
#' 
analyze_features <- function(features, tmt, learner,
                             task_type = "binary",
                             weight = NULL, y = NULL, block = NULL,
                             cluster = NULL,
                             loss = list(rmse = loss_funs$rmse),
                             test_method = "holdout",
                             tune_split = 5L,
                             test_split = ifelse(test_method == "holdout",
                                                 0.3, NA),
                             permutations = 100L, parallel = NULL,
                             random_seed = 1, verbose = FALSE) {
  
  start_time <- Sys.time()
  set.seed(random_seed, kind = "L'Ecuyer-CMRG")
  
  # Defensive Checks --------------------------------------------------------
  say(verbose, "Checking inputs...")
  task_type <- check_task_type(task_type)
  
  features <- check_features(features)
  tmt      <- check_tmt(tmt, task_type)
  weight   <- check_weight(weight)
  y        <- check_y(y)
  
  block   <- check_block(block)
  cluster <- check_cluster(cluster)
  learner <- check_learner(learner)
  loss    <- check_loss(loss, task_type)
  
  test_method  <- check_method(test_method)
  tune_split   <- check_tune_split(tune_split)
  test_split   <- check_test_split(test_split, test_method)
  permutations <- check_permutations(permutations, task_type)
  parallel     <- check_parallel(parallel)
  
  check_block_cluster(block, cluster)
  
  
  # Preprocess Arguments ----------------------------------------------------
  if (is.matrix(features)) features <- list(features)
  
  feat_tmt_match <- purrr::map_lgl(features, ~nrow(.x) == length(tmt))
  if (any(feat_tmt_match == FALSE)) { 
    rlang::abort("nrow(features) and length(tmt) must be the same")
  }
  
  if (length(features) > 1) {
    names(features) <- names(features) %||% 
      paste0("feature_set_", seq_along(features))
  }
  
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
  
  # Permutations ------------------------------------------------------------
  if (rlang::is_null(permutations) | rlang::is_list(permutations)) { 
    permuted_tmt <- permutations
  }
  
  perm_mask <- switch(test_method,
                      holdout = which(folds$test == 2),
                      # Default:
                      which(!is.na(folds$tune)))
  
  if (rlang::is_scalar_integerish(permutations)) {
    
    say(verbose, "Permuting treatment assignment...")
    permuted_tmt <- permute_tmt(tmt[perm_mask],
                                n_perm  = permutations,
                                block   = block[perm_mask],
                                cluster = cluster[perm_mask])
  }
  
  # Analyze -----------------------------------------------------------------
  results <- purrr::imap(features, analyze_feature_set,
                         tmt          = tmt,
                         learner      = learner,
                         task_type    = task_type,
                         weight       = weight,
                         y            = y,
                         loss         = loss,
                         test_method  = test_method,
                         folds        = folds,
                         permuted_tmt = permuted_tmt,
                         parallel     = parallel,
                         verbose      = verbose)
  
  # Combine -----------------------------------------------------------------
  result <- list(learners    = purrr::map(results, "learner"),
                 folds       = folds,
                 predictions = purrr::map(results, "predictions"),
                 permuted_predictions = purrr::map(results,
                                                   "permuted_predictions"),
                 perm_test      = Reduce(rbind, 
                                         purrr::map(results, "perm_test")),
                 tmt_hat_struct = Reduce(rbind,
                                         purrr::map(results, "tmt_hat_struct")))
  
  # Compare Feature Sets  ----------------------------------------------
  if (length(features) > 1) say(verbose, "Comparing feature sets...")
  result$comparison <- compare_loss(result$perm_test)
  
  
  # Metadata ----------------------------------------------------------------
  result$run_time    <- Sys.time() - start_time
  result$random_seed <- random_seed
  
  # Return ------------------------------------------------------------------
  result
}

#' @rdname analyze
#' @export
analyse_features <- analyze_features