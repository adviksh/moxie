#' Divide Data into Tuning and Training Sets
#' @export
#' 
#' @inheritParams analyze
make_folds <- function(test_method, tune_split, test_split, task_type, tmt,
                       block = NULL, cluster = NULL) {
  
  test_method <- check_method(test_method)
  task_type   <- check_task_type(task_type)
  tune_split  <- check_tune_split(tune_split)
  if (test_method == "holdout") {
    test_split  <- check_test_split(test_split, test_method)
  }
  
  switch(test_method,
         holdout = make_folds_holdout(test_split, tune_split, task_type, tmt,
                                      block, cluster),
         # Default:
         make_folds_cv(tune_split, task_type, tmt, block, cluster))
}

make_folds_holdout <- function(test_split, tune_split, task_type, tmt, block,
                               cluster) {
  
  folds  <- list(tune = tune_split, test = test_split)
  strata <- switch(task_type,
                   binary = tmt,
                   multiclass = tmt,
                   regression = rep_len(1L, length(tmt)))
  
  # Make training folds if necessary
  if (rlang::is_scalar_double(test_split)) {
    
    fold_wt <- c(1 - test_split, test_split)
    
    if (is_not_null(block)) {
      
      folds$test <- fold_grouped(n_folds = 2,
                                 fold_weights = fold_wt,
                                 groups = block)
      
    } else if (is_not_null(cluster)) {
      
      folds$test <- fold_grouped_stratified(n_folds = 2,
                                            fold_weights = fold_wt,
                                            groups = cluster,
                                            strata = strata,
                                            .f = utils::head, n = 1)
    } else {
      
      folds$test <- fold_stratified(n_folds = 2,
                                    fold_weights = fold_wt,
                                    strata = strata)
    }
  }
  
  # Make tuning folds if necessary
  if (rlang::is_scalar_integerish(tune_split)) {
    
    train_mask <- folds$test == 1
    folds$tune <- rep(NA_integer_, length(tmt))
    
    if (is_not_null(block)) {
      
      folds$tune[train_mask] <- fold_grouped(n_folds = tune_split,
                                             groups = block[train_mask])
      
    } else if (is_not_null(cluster)) {
      
      folds$tune[train_mask] <- fold_grouped_stratified(n_folds = tune_split,
                                                        groups = cluster[train_mask],
                                                        strata = strata[train_mask],
                                                        .f = utils::head, n = 1)
    } else {
      
      folds$tune[train_mask] <- fold_stratified(n_folds = tune_split,
                                                strata = strata[train_mask])
    }
  }
  
  return(folds)
}

make_folds_cv <- function(tune_split, task_type, tmt, block, cluster) {
  
  folds <- list(tune = tune_split, test = tune_split)
  strata <- switch(task_type,
                   binary     = tmt,
                   multiclass = tmt,
                   regression = rep_len(1L, length(tmt)))
  
  if (rlang::is_scalar_integerish(tune_split)) {
    if (is_not_null(block)) {
      
      folds$tune <- fold_grouped(n_folds = tune_split,
                                 groups  = block)
      
    } else if (is_not_null(cluster)) {
      
      folds$tune <- fold_grouped_stratified(n_folds = tune_split,
                                            groups  = cluster,
                                            strata  = strata,
                                            .f = utils::head, n = 1)
    } else {
      
      folds$tune <- fold_stratified(n_folds = tune_split,
                                    strata  = strata)
    }
    
    folds$test <- folds$tune
  }
  
  return(folds)
}