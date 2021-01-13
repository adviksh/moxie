#' @title Check Arguments
#' @name check_arguments
#' @inheritParams analyze
#' @description Utilities to checks argument for compatibility with this
#' package's expectations for inputs. They will throw errors if expectations
#' are violated.
NULL

#' @export
#' @rdname check_arguments
check_task_type <- function(task_type) {
  
  switch(task_type,
         binary = task_type,
         regression = task_type,
         multiclass = task_type,
         # Else:
         abort_one_of(what = "task_type",
                      one_of = c("binary", "regression", "multiclass"),
                      not = task_type))
}

#' @export
#' @rdname check_arguments
check_features <- function(features) {
  
  UseMethod("check_features")
  
  return(features)
}

check_features.default <- function(features) {
  
  msg <- paste("features must be a matrix or a list of matrices.",
               "You provided features as an object with classes:",
               commas(class(features)))
  
  rlang::abort("error_s3_class", message = msg)
  
}

check_features.matrix <- function(features) {
  
  if (anyNA(features)) { abort_na(what = "features") }
  
  if (rlang::is_integer(features) == FALSE &&
      rlang::is_double(features) == FALSE) {
    
    msg <- paste("features must have only numeric columns.",
                 "features cannot have non-numeric columns, like factors.",
                 "Please encode factors numerically (ex: with dummy variables).")
    
    rlang::abort("error_s3_class", message = msg)
    
  }
  
  return(features)
  
}

check_features.list <- function(features) {
  
  if (is.data.frame(features)) {
    
    rlang::inform("conversion",
                  message = "Converting features from data.frame to matrix.")
    
    features <- as.matrix(features)
    
    return(check_features(features))
  }
  
  if (any(purrr::map_lgl(features, is.matrix) == FALSE)) {
    
    msg <- paste("If provided as a list, features must be a list of matrices.",
                 "These list elements are not matrices:",
                 commas(which(purrr::map_lgl(features, is.matrix) == FALSE)))
    
    rlang::abort("error_s3_class", message = msg)
    
  }
  
  if (rlang::is_named(features) && 
      rlang::is_dictionaryish(features) == FALSE) {
    
    duped_names <- unique(names(features)[duplicated(names(features))])
    
    msg <- paste("features is a named list, but names are not unique.",
                 "These names are duplicated: ",
                 commas(duped_names))
    
    rlang::abort(message = msg)
    
  }
  
  return(features)
  
}

#' @export
#' @rdname check_arguments
check_tmt <- function(tmt, task_type) {
  
  if (anyNA(tmt)) abort_na(what = "tmt", where = which(is.na(tmt)))
  
  switch(task_type,
         binary = check_tmt_binary(tmt),
         regression = check_tmt_regr(tmt),
         multiclass = check_tmt_multi(tmt),
         # Else:
         abort_one_of(what = "task_type",
                      one_of = c("binary", "regression", "multiclass"),
                      not = task_type))
  
}

check_tmt_binary <- function(tmt) {
  
  if (is.factor(tmt) && length(levels(tmt)) != 2) {
    
    msg <- paste("If passed as a factor, tmt must have exactly two levels.",
                 "It has", length(levels(tmt)), "levels.")
    
    rlang::abort("error_data", message = msg)
    
  } else {
    
    if (setequal(tmt, 0) | setequal(tmt, 1)) { 
      
      msg <- paste("tmt must contain both positive and negative cases.",
                   "It only contains one kind of case case:", unique(tmt))
      
      rlang::abort("error_data", message = msg)
    }
    
    if (setequal(tmt, 0:1) == FALSE) {
      msg <- paste("If passed as an integer or numeric, tmt must only",
                   "contain the values 0 and 1.")
      
      rlang::abort("error_data", message = msg)
    }
    
  }
  
  return(tmt)
}

check_tmt_regr <- function(tmt) { return(tmt) }

check_tmt_multi <- function(tmt) { return(tmt) }

#' @export
#' @rdname check_arguments
check_weight <- function(weight) {
  
  if (is.null(weight)) return(NULL)
  
  if (is.numeric(weight) == FALSE) {
    
    msg <- paste("weight must be a numeric vector. weight must not have other",
                 "classes. The provided weight has these classes:",
                 commas(class(weight)), ".") 
    
    rlang::abort("error_data", message = msg)
  }
  
  if (any(weight < 0)) {
    
    msg <- paste("weight must be zero or greater for all observations.",
                 "It is negative for observations:",
                 commas(which(weight < 0)))
    
    rlang::abort("error_data", message = msg)
  }
  
  if (anyNA(weight)) abort_na(what = "weight", where = which(is.na(weight)))
  
  return(weight)
}

#' @export
#' @rdname check_arguments
check_y <- function(y) {
  
  if (is.null(y)) return(y)
  
  if (is.matrix(y) == FALSE) { 
    rlang::abort("error_s3_class", message = "y must be a matrix.")
  }
  
  if (anyNA(y)) abort_na(what = "y")
  
  return(y)
}

#' @export
#' @rdname check_arguments
check_block <- function(block) {
  
  if (is.null(block)) return(block)
  
  if (anyNA(block)) abort_na(what = "block", where = which(is.na(block)))
  
  return(block)
}

#' @export
#' @rdname check_arguments
check_cluster <- function(cluster) {
  
  if (is.null(cluster)) return(cluster)
  
  if (anyNA(cluster)) abort_na(what = "cluster", where = which(is.na(cluster)))
  
  return(cluster)
}

#' @export
#' @rdname check_arguments
check_block_cluster <- function(block, cluster) {
  
  if (is.null(block) || is.null(cluster)) return(NULL)
  
  # Get all blocks associated with each cluster
  cluster_block <- tapply(block, cluster, unique, simplify = FALSE)
  cluster_block_count <- purrr::map_int(cluster_block, length)
  
  if (any(cluster_block_count > 1)) {
    
    msg <- paste("All clusters must belong to only one block.",
                 "These clusters belong to more than one block:",
                 commas(names(cluster_block)[cluster_block_count > 1]))
    
    rlang::abort("error_data", message = msg)
  }
  
  return(NULL)
}

#' @export
#' @rdname check_arguments
check_method <- function(test_method) {
  
  if (! test_method %in% c("in_sample", "holdout", "cv")) {
    abort_one_of(what = "test_method",
                 one_of = c("in_sample", "holdout", "cv"),
                 not = test_method)
  }
  
  return(test_method)
}

#' @export
#' @rdname check_arguments
check_test_split <- function(test_split, test_method) {
  
  if (test_method == "holdout") {
    
    if(!is_lengthy_integerish(test_split) && !is_proportion(test_split)) {
      rlang::abort("test_split must be a proportion or vector of integers")
    }
    
  }
  
  return(test_split)
}

#' @export
#' @rdname check_arguments
check_tune_split <- function(tune_split) {
  
  if(!rlang::is_integerish(tune_split)) {
    rlang::abort("tune split must be an integer or vector of integers")
  }
  
  return(tune_split)
}

#' @export
#' @rdname check_arguments
check_learner <- function(learner) {
  
  if (learners::is_learner_constructor(learner)) learner <- learner()
  
  if (learners::is_learner(learner) == FALSE) {
    rlang::abort("learner must have class 'learner'.")
  }
  
  tune_fun    <- learner$tune_fun
  train_fun   <- learner$train_fun
  model      <- learner$model
  predict_fun <- learner$predict_fun
  
  if (is.null(tune_fun) & is.null(train_fun) & is.null(model)) {
    msg <- paste("learner must have at least one of the following:", 
                 "(1) a tuning function in the `tune_fun` slot",
                 "(2) a training function in the `train_fun` slot.",
                 "(3) a model in the `model` slot.",
                 "All are empty.")
    rlang::abort("error_learner", message = msg)
  }
  
  return(learner)
}

#' @export
#' @rdname check_arguments
check_loss <- function(loss, task_type) {
  
  if (rlang::is_list(loss)) { 
    return(lapply(loss, check_loss, task_type = task_type))
  }
  
  if (!is.function(loss)) { rlang::abort("loss must be a function.") }
  
  if (identical(arg_vec(loss), c("actual", "predicted", "weight")) == FALSE) {
    msg <- paste("loss must take only these arguments in order: actual,", 
                 " predicted, weight. The provided loss takes these arguments:",
                 commas(arg_vec(loss)))
    rlang::abort(message = msg)
  }
  
  switch(task_type,
         binary = check_loss_binary(loss),
         regression = check_loss_regr(loss),
         multiclass = check_loss_multi(loss),
         abort_one_of(what = "task_type",
                      one_of = c("binary", "regression", "multiclass"),
                      not = task_type))
  
}

check_loss_binary <- function(loss) {
  
  if (loss(c(0,1), c(1,0)) < loss(c(0,1), c(0,1))) {
    msg <- paste("loss should be a function for which lower values are more",
            "desirable. Did you provide a reward function instead?")
    rlang::warn(message = msg)
  }
  
  return(loss)
  
}

check_loss_regr <- function(loss) {
  
  if (loss(c(0,1), c(1,0)) < loss(c(0,1), c(0,1))) {
    msg <- paste("loss should be a function for which lower values are more",
            "desirable. Did you provide a reward function instead?")
    rlang::warn(message = msg)
  }
  
  return(loss)
  
}

check_loss_multi <- function(loss) {
  
  msg <- paste("check_loss not implemented for multiclass classification.",
                 "Returning loss function without checking it.")
  rlang::inform(message = msg)
  
  return(loss)
  
}

#' @export
#' @rdname check_arguments
check_permutations <- function(permutations, task_type) {
  if (rlang::is_null(permutations) |
      rlang::is_scalar_integerish(permutations)) {
    return(permutations) 
  }
  
  if (rlang::is_list(permutations) == FALSE) {
    msg <- paste("permutations must be an integer or a list of permuted",
                 "treatment vectors. The provided permutations have classes:",
                 commas(class(permutations)))
    rlang::abort("error_s3_class", message = msg)
  }
  
  permutations <- lapply(permutations, check_tmt, task_type = task_type)
  
  return(permutations)
}

#' @export
#' @rdname check_arguments
check_parallel <- function(parallel) {
  if (is.null(parallel)) return(parallel)
  
  known_vals <- c("tune", "train", "predict", "perm_test", "perm_pred",
                  "tmt_hat_struct")
  unknown_vals <- setdiff(parallel, known_vals)
  
  if (length(unknown_vals) > 0) {
    msg <- paste("Can only parallelize at these known levels:",
                 commas(known_vals), ".",
                 "Ignoring these provided levels:",
                 commas(unknown_vals), ".")
    rlang::inform(message = msg)
  }
  
  return(parallel)
}