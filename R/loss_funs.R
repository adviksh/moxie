rmse <- function(actual, predicted, weight = rep(1, length(actual))) { 
  
  sqrt(mse(actual, predicted, weight))
  
} 

mse <- function(actual, predicted, weight = rep(1, length(actual))) { 
  
  if (is.factor(actual)) actual <- as.integer(actual) - 1L
  
  return( stats::weighted.mean((actual - predicted)^2, weight) )
  
}

roc_area_above <- function(actual, predicted, weight = rep(1, length(actual))) {
  
  if (is.factor(actual)) actual <- as.integer(actual) - 1L
  
  # calculate auc twice; once counting all ties in favor of predictions
  # and one counting all ties against predictions
  auc_hi <- roc_area_under(actual, predicted, weight, order_by = actual)
  auc_lo <- roc_area_under(actual, predicted, weight, order_by = 1 - actual)
  
  auc <- (auc_hi + auc_lo) / 2
  
  return(1 - auc)
  
}

# Helper to calculate are above roc curve
roc_area_under <- function(actual, predicted, weight, order_by) {
  ord <- order(predicted, order_by)
  
  ac_sort <- actual[ord]
  pr_sort <- predicted[ord]
  wt_sort <- weight[ord]
  
  wt_sort_1 <- wt_sort[ac_sort == 1]
  
  cumsum_wt_all <- cumsum(wt_sort)
  cumsum_wt_1   <- cumsum(wt_sort_1)
  
  sum_wt_0 <- cumsum_wt_1[length(cumsum_wt_1)]
  sum_wt_1 <- cumsum_wt_all[length(cumsum_wt_all)] - sum_wt_0
  
  log_numer <- log(sum(wt_sort_1 * (cumsum_wt_all[ac_sort == 1] - cumsum_wt_1)))
  auc <- exp(log_numer - log(sum_wt_0) - log(sum_wt_1))
  
  return(auc)
}

neg_cov <- function(actual, predicted, weight = rep(1, length(actual))) {
  mean_actual <- stats::weighted.mean(actual, weight)
  mean_pred   <- stats::weighted.mean(predicted, weight)
  
  resid_actual <- actual - mean_actual
  resid_pred   <- predicted - mean_pred
  
  nc <- -1 * (weight * resid_actual) %*% resid_pred / (sum(weight) - 1)
  as.numeric(nc)
}

neg_bern_loglik <- function(actual, predicted, weight = rep(1, length(actual))) {
  sum(weight * stats::dbinom(actual, size = 1, predicted, log = TRUE))
}

#' Common Loss Functions
#' @export
#' @details loss functions take three arguments: (actual, predicted, weight),
#' and return a loss. Current loss functions include
#' \itemize{
#'   \item rmse: root-mean-squared error
#'   \item mse: mean-squared error
#'   \item roc_area_above: the area above the receiver operating curve. This is
#'   the empirical probability that a randomly selected control unit receives
#'   a higher predicted treatment status than a randomly selected treatment
#'   unit.
#'   \item neg_cov: the negative covariance between predicted and observed
#'   treatment status.
#'   \item neg_bern_loglik: the negative Bernoulli log-likelihood
#' }
loss_funs <- list(
  rmse = rmse,
  mse = mse,
  roc_area_above = roc_area_above,
  neg_cov = neg_cov,
  neg_bern_loglik = neg_bern_loglik
)