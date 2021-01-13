#' @rdname analyze
#' @import learners
#' 
#' @param feature_set mtx
#' @param feature_set_name character
#' @param folds list(tune = integer(), train = integer())
#' @param permuted_tmt NULL | list(integer())
#' 
analyze_feature_set <- function(feature_set, feature_set_name, tmt, learner,
                                task_type, weight, y, loss, test_method,
                                folds, permuted_tmt, parallel, verbose) {
  
  # Parallel Setup -------------------------------------------------------------
  perm_pred_map <- ifelse("perm_pred" %in% parallel,
                          furrr::future_map, purrr::map)
  
  # Training Sets -----------------------------------------------------------
  fit_idx <- switch(test_method,
                    holdout = which(folds$test == 1),
                    # Default:
                    which(is.na(folds$tune) == FALSE))
  
  test_idx <- switch(test_method,
                     holdout = which(folds$test == 2),
                     # Default:
                     fit_idx)
  
  # Tune --------------------------------------------------------------------
  if (is_not_null(learner$tune_fun)) { say(verbose, "Tuning learner(s)...") }
  tuned_learner <- learners::tune(object     = learner,
                                  features   = feature_set[fit_idx, , drop = FALSE],
                                  tgt        = tmt[fit_idx],
                                  wt         = weight[fit_idx],
                                  tune_folds = folds$tune[fit_idx])
  
  # Train -------------------------------------------------------------------
  say(verbose, "Training learner(s)...")
  trained_learner <- learners::train(object   = tuned_learner,
                                     features = feature_set[fit_idx, , drop = FALSE],
                                     tgt      = tmt[fit_idx],
                                     wt       = weight[fit_idx])
  
  # Predict -----------------------------------------------------------------
  predictions <- switch(test_method,
                        cv = learners::tune_predict_oos(object     = learner,
                                                        features   = feature_set,
                                                        tgt        = tmt,
                                                        wt         = weight,
                                                        tune_folds = folds$tune),
                        # Default:
                        stats::predict(trained_learner, feature_set))
  
  # Permutations ------------------------------------------------------------
  if (test_method %in% c("in_sample", "cv")) {
    say(verbose, "Tuning learners with permuted treatment...")
  }
  
  permuted_pred <- switch(test_method,
                          holdout   = NULL,
                          in_sample = perm_pred_map(permuted_tmt, 
                                                    learners::tune_predict_ins,
                                                    object     = learner,
                                                    features   = feature_set[fit_idx, , drop = FALSE],
                                                    wt         = weight[fit_idx],
                                                    tune_folds = folds$tune[fit_idx]),
                          cv = perm_pred_map(permuted_tmt, 
                                             learners::tune_predict_oos,
                                             object     = learner,
                                             features   = feature_set[fit_idx, , drop = FALSE],
                                             wt         = weight[fit_idx],
                                             tune_folds = folds$tune[fit_idx]))
  
  perm_test_result <- perm_test(pred          = predictions[test_idx],
                                permuted_pred = permuted_pred,
                                tmt           = tmt[test_idx],
                                permuted_tmt  = permuted_tmt,
                                weight        = weight[test_idx],
                                loss          = loss)
  
  if (is.data.frame(perm_test_result)) {
    
    perm_test_result <- cbind(data.frame(feature_set = rep(feature_set_name,
                                                           length(loss)),
                                         stringsAsFactors = FALSE),
                              perm_test_result)
    
  }
  
  # Summarize T Hat ----------------------------------------------
  if (is_not_null(y)) say(verbose, "Summarizing predicted treatment status...")
  tmt_hat_struct <- summarize_tmt_hat_struct(tmt_hat   = predictions,
                                             tmt       = tmt,
                                             y         = y,
                                             subset    = test_idx,
                                             task_type = task_type)
  
  # Return ------------------------------------------------------------------
  list(learner = trained_learner,
       predictions = predictions,
       permuted_predictions = permuted_pred,
       perm_test = perm_test_result,
       tmt_hat_struct = tmt_hat_struct)
}