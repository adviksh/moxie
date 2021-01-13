#' Estimate Treatment Effect Given Outcomes and Treatment Predictions
#' @export
#' @inheritParams analyze
#' 
#' @param tmt_hat vector | matrix \cr
#' Estimated treatment assignment. Probability of treatment assignment for
#' binary tasks, dosage for continuous tasks, and matrix of control and
#' treatment probabilities for multiclass tasks (one probability per column).
#' @param subset vector \cr
#' Optional, a logical or integer vector identifying which observations to
#' use when estimating treatment effects.
#' 
summarize_tmt_hat_struct <- function(tmt_hat, y, tmt, subset, task_type) {
  
  task_type <- check_task_type(task_type)
  tmt       <- check_tmt(tmt, task_type)
  
  estimator <- switch(task_type,
                      binary         = summarize_tmt_hat_struct_binary,
                      regression     = summarize_tmt_hat_struct_regr,
                      multiclass     = summarize_tmt_hat_struct_multi)
  
  estimator(tmt_hat, y, tmt, subset)
}

summarize_tmt_hat_struct_binary <- function(tmt_hat, y, tmt, subset) {
  
  # Fast Returns ------------------------------------------------------------
  if (is.null(y)) return(NULL)
  
  
  # Process Inputs ----------------------------------------------------------
  if (is.matrix(y) == FALSE) { y <- as.matrix(y) }
  if (is.vector(tmt_hat)) { 
    tmt_hat <- cbind(1 - tmt_hat, tmt_hat)
    colnames(tmt_hat) <- sort(unique(tmt))
  }
  
  colnames(y) <- colnames(y) %||% paste0("Y", seq_len(ncol(y)))
  
  mask <- subset %||% rep(TRUE, length(tmt))
  
  y <- y[mask, , drop = FALSE]
  tmt_hat <- tmt_hat[mask, , drop = FALSE]
  
  # Function Body -----------------------------------------------------------
  tau_tb <- expand.grid(outcome_name     = colnames(y),
                        treatment_group  = colnames(tmt_hat),
                        stringsAsFactors = FALSE)
  
  tau_tb$group_mean <- purrr::map2_dbl(tau_tb$treatment_group,
                                       tau_tb$outcome_name,
                                       ~weighted.mean(y[,.y], tmt_hat[,.x]))
  
  tau_tb$group_sd <- purrr::map2_dbl(tau_tb$treatment_group,
                                     tau_tb$outcome_name,
                                     ~weighted_sd(y[,.y], tmt_hat[,.x]))
  
  ctrl_stats <- subset(tau_tb,
                       subset = tau_tb$treatment_group == colnames(tmt_hat)[1],
                       select = c("outcome_name", "group_mean", "group_sd"))
  ctrl_stats <- stats::setNames(ctrl_stats,
                                c("outcome_name", "control_mean", "control_sd"))
  
  tau_tb <- merge(tau_tb, ctrl_stats, by = c("outcome_name"))
  
  # Get the treatment effects by subtracting the control vector from each mean vector
  tau_tb$delta_mean <- tau_tb$group_mean - tau_tb$control_mean
  tau_tb$delta_sd   <- tau_tb$group_sd - tau_tb$control_sd
  
  return(tau_tb)
}

summarize_tmt_hat_struct_regr <- function(tmt_hat, y, tmt, subset) {
  message("Treatment effect estimation not implemented for continuous ",
          "treatment. Returning NULL.")
  return(NULL)
}

summarize_tmt_hat_struct_multi <- function(tmt_hat, y, tmt, subset) {
  message("Treatment effect estimation not implemented for multiclass ",
          "treatment. Returning NULL.")
  return(NULL)
}