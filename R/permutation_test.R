#' Perform a Permutation Test
#' @export
#' 
#' @inheritParams analyze
#' 
#' @details If the observed tmt vector is included in the permuted_tmt list,
#' then the p-value returned is mean(permuted_loss <= observed_loss). But, if
#' the observed tmt vector is not in the permuted_tmt list, the p-value
#' returned is mean(c(observed_loss, permuted_loss) <= observed_loss). This
#' ensures that the estimated p-value is never exactly 0.
#' 
#' @param pred numeric | matrix  \cr
#' a) For purely binary classification losses: The predicted probabilities for 
#' the positive class as a numeric vector. b) For multiclass classification 
#' losses: The predicted probabilities for all classes, always as a 
#' numeric matrix, where columns are named with class labels.
#' @param permuted_tmt list \cr
#' A list of permuted treatment indicators
#' @param permuted_pred NULL | list \cr
#' A list of permuted predictions
#'
perm_test <- function(tmt, pred,
                      permuted_tmt  = NULL,
                      permuted_pred = NULL,
                      weight  = NULL,
                      loss = list(rmse = loss_funs$rmse)) {
  
  # Fast Return -------------------------------------------------------------
  if (rlang::is_null(permuted_tmt)) { return(NULL) }
  
  # Defense -----------------------------------------------------------------
  if (is.vector(pred) == FALSE & is.matrix(pred) == FALSE) {
    rlang::abort(message = "pred must be a vector or matrix",
                 .subclass = "error_s3_class")
  }
  
  if (rlang::is_list(permuted_tmt) == FALSE) {
    rlang::abort(message = "permuted_tmt must be a list",
                 .subclass = "error_s3_class")
  }
  
  if (is_not_null(permuted_pred) & rlang::is_list(permuted_pred) == FALSE) {
    rlang::abort(message = "if provided, permuted_pred must be a list",
                 .subclass = "error_s3_class")
  }
  
  if (is_not_null(permuted_pred) & 
      length(permuted_tmt) != length(permuted_pred)) {
    msg <- paste("if provided, permuted_pred must be the same length (",
                 length(permuted_pred), ") as permuted_tmt (", length(permuted_tmt),
                 ")")
    rlang::abort(message = msg,
                 .subclass = "error_s3_class")
  }
  
  # Parse Args --------------------------------------------------------------
  weight <- weight %||% rep(1, length(tmt))
  
  if (is.factor(tmt)) { tmt <- as.integer(tmt) - 1L }
  
  permuted_tmt <- purrr::map(permuted_tmt,
                             function(x) {
                               if (is.factor(x)) x <- as.integer(x) - 1L
                               return(x)
                             })
  
  if (rlang::is_function(loss)) { loss <- list(loss) }
  
  if (rlang::is_named(loss) == FALSE) {
    
    old_names <- names(loss) %||% rep_len("", length(loss))
    new_names <- ifelse(old_names == "",
                        paste0("loss_fun_", seq_along(loss)),
                        old_names)
    
    loss <- stats::setNames(loss, new_names)
  }
  
  # Calculate Losses and Feature Set Performance ---------------------------
  loss_observed <- purrr::map_dbl(loss,
                                  ~.x(actual = tmt,
                                      predicted = pred,
                                      weight = weight))
  
  if (rlang::is_null(permuted_pred)) {
    loss_permuted <- purrr::map(loss,
                                ~purrr::map_dbl(permuted_tmt, .x,
                                                predicted = pred,
                                                weight = weight))
  } else {
    loss_permuted <- purrr::map(loss,
                                ~purrr::map2_dbl(permuted_tmt, permuted_pred,
                                                 .x,
                                                 weight = weight))
  }
  
  # If the observed treatment vector is not among the permutations,
  # add the observed loss to the permuted losses. This avoids returning p = 0.
  if ((list(tmt) %in% permuted_tmt) == FALSE) {
    loss_permuted <- purrr::map2(loss_observed, loss_permuted, c)
  }
  
  perm_test_result <- data.frame(loss_fun = names(loss),
                                 observed_loss = loss_observed,
                                 stringsAsFactors = FALSE)
  perm_test_result$permuted_loss <- loss_permuted
  perm_test_result$p_val <- purrr::map2_dbl(loss_observed,
                                            loss_permuted,
                                            perm_p_val)
  
  return(perm_test_result)
  
}
