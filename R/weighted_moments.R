#' Weighted Sampling Variance and SD
#' @description Unbiased estimator of sampling variance, with weights.
#' @param x an object containing the values whose weighted variance or standard deviation
#' to calculate.
#' @param w a numerical vector of weights the same length as x giving the weights to use 
#' for elements of x.
#' @param na.rm a logical value indicating whether NA values in x should be stripped
#' before computaiton.
weighted_var <- function(x, w, na.rm = FALSE) {
  
  # Defensive Checks --------------------------------------------------------
  if (anyNA(w)) abort_na(what = "w", where = which(is.na(w)))
  
  if (length(x) != length(w)) {
    msg <- paste("w must have as many entries (", length(w), 
                 ") as x (", length(x), ")")
    rlang::abort(message = msg)
  }
  
  # Function Body -----------------------------------------------------------
  if (na.rm) {
    na_idx <- is.na(x)
    
    if (sum(w[!na_idx]) <= 1) {
      msg <- "weights must sum to at least 1 after removing NAs"
      rlang::abort(message = msg)
    }
    
    x <- x[!na_idx]
    w <- w[!na_idx]
  }
  
  wt_mean <- stats::weighted.mean(x, w, na.rm = na.rm)
  wt_ssr <- sum(w * (x - wt_mean)^2)
  wt_ssr / (sum(w) - 1)
}

#' @rdname weighted_var
weighted_sd <- function(x, w, na.rm = TRUE) { sqrt(weighted_var(x, w, na.rm)) }