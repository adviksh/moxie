#' CV Folds
#' @export
#'
#' @param n_obs Number of Observations
#' @param fold_weights Nonnegative vector giving the relative size of each fold
#' @param n_folds Number of Folds
#' @param groups vector identifying the group each observation belongs to
#' @param strata vector identifying the stratum each observation belongs to
#' @param .f A function to identify the stratum for each group. See details.
#' @param ... Additional arguments to .f
#' 
#' @return An integer vector giving the fold for each observation
#'
#' @examples
#' # Basic use
#' fold(n_folds = 5, n_obs = 20)
#'
#' # The first fold will have twice as many elements as the second
#' fold(n_folds = 2, fold_weights = c(2, 1), n_obs = 3)
#'
fold <- function(n_folds, fold_weights = rep(1, n_folds), n_obs) {
  
  # Defensive Checks -----------------------------------------------------------
  # Argument Types
  if (rlang::is_scalar_integerish(n_obs) == FALSE) {
    rlang::abort("n_obs must be a single positive integer")
  }
  
  if (rlang::is_scalar_integerish(n_folds) == FALSE) {
    rlang::abort("n_obs must be a single positive integer")
  }
  
  if (is.numeric(fold_weights) == FALSE) {
    rlang::abort("fold_weights must be a numeric vector")
  }  
  
  # Argument Properties
  if (n_folds < 1L) {
    rlang::abort("n_folds must be greater than or equal to 1.")
  }
  
  if (n_folds > n_obs) {
    msg <- paste("n_folds (", n_folds, ") must be greater than or equal to n_obs (",
                 n_obs, ")")
    rlang::abort(message = msg)
  }
  
  if (any(fold_weights < 0)) {
    msg <- paste("All fold weights must be greater than or equal to 0. These entries",
                 "are not: ", commas(which(fold_weights < 0)))
    rlang::abort(message = msg)
  }
  
  if (length(fold_weights) != n_folds) {
    message <- paste("fold_weights must have the same length (",
                     length(fold_weights), ") as n_folds (", n_folds, ")")
    rlang::abort(message = msg)
  }
  
  # Function Body --------------------------------------------------------------
  # Get the approximate number of observations per fold, forcing each fold to
  # have at least 1 observation
  fold_share <- fold_weights / sum(fold_weights)
  fold_n     <- round(n_obs * fold_share)
  fold_n     <- pmax(fold_n, 1)
  
  # Resize the smallest or largest folds until the sum of fold sizes equals the
  # number of observations
  while(sum(fold_n) < n_obs) { fold_n[which.min(fold_n)] <- min(fold_n) + 1 }
  while(sum(fold_n) > n_obs) { fold_n[which.max(fold_n)] <- max(fold_n) - 1 }
  
  # Create a vector of folds using the per-fold counts, then scramble the order
  folds <- rep(seq_len(n_folds), times = fold_n)
  folds <- sample(folds)
  
  
  # Return ---------------------------------------------------------------------
  return(folds)
}

#' @describeIn fold Grouped CV Folds
#' @export
#' 
fold_grouped <- function(n_folds, fold_weights = rep(1, n_folds), groups) {
  
  # Defensive Checks -----------------------------------------------------------
  if (n_folds > length(unique(groups))) {
    rlang::abort("n_folds (", n_folds, ") can't be greater than the number of ",
         "groups (", length(unique(groups)), ")")
  }
  
  
  # Function Body --------------------------------------------------------------
  # Divide the groups into folds
  group_levels <- unique(groups)
  group_folds  <- fold(n_folds = n_folds,
                       n_obs = length(group_levels),
                       fold_weights = fold_weights)
  
  # Match observations to their group's fold
  folds <- group_folds[match(groups, group_levels)]
  
  # Return  --------------------------------------------------------------------
  return(folds)
  
}

#' @describeIn fold Stratified CV Folds
#' @export
#' 
fold_stratified <- function(n_folds, fold_weights = rep(1, n_folds), strata) {
  
  # Defensive Checks -----------------------------------------------------------
  if (n_folds > min(table(strata))) {
    rlang::abort("n_folds (", n_folds, ") can't be greater than the number of ",
         "observations in the smallest stratum (", min(table(strata)), ")")
  }
  
  
  # Function Body --------------------------------------------------------------
  # Identify the observationsin each stratum
  strata_indices <- split(seq_along(strata), strata)
  strata_sizes   <- lapply(strata_indices, length)
  
  # Split each stratum into folds
  # Pass all arguments except n_obs so that strata_sizes gets passed as n_obs
  strata_folds <- lapply(strata_sizes, fold,
                         n_folds = n_folds,
                         fold_weights = fold_weights)
  
  strata_folds   <- unlist(strata_folds, recursive = FALSE, use.names = FALSE)
  strata_indices <- unlist(strata_indices, recursive = FALSE, use.names = FALSE)
  
  # Match each observation to its fold
  folds <- rep(NA_integer_, length(strata))
  folds[strata_indices] <- strata_folds
  
  # Return ---------------------------------------------------------------------
  return(folds)
  
}

#' @describeIn fold Grouped and Stratified CV Folds
#' @export
#' 
fold_grouped_stratified <- function(n_folds, fold_weights = rep(1, n_folds),
                                    groups, strata, .f, ...) {
  
  # Defensive Checks -----------------------------------------------------------
  if (is.function(.f) == FALSE) {
    rlang::abort(".f must be a function.")
  }
  
  if (length(strata) != length(groups)) {
    rlang::abort("Group vector and strata vector must have the same length.")  
  }
  
  if (n_folds > length(unique(groups))) {
    rlang::abort("n_folds (", n_folds, ") can't be greater than the number of ",
         "groups (", length(unique(groups)), ")")
  }
  
  
  # Function Body --------------------------------------------------------------
  # Divide the groups into folds
  group_levels <- unique(groups)
  
  # Identify the stratum for each group
  group_strata <- tapply(strata, groups, .f, simplify = FALSE, ...)
  group_strata <- group_strata[as.character(group_levels)]
  group_strata <- unlist(group_strata, use.names = FALSE)
  
  # Put each group in folds by stratum
  group_folds  <- fold_stratified(n_folds = n_folds,
                                  strata = group_strata,
                                  fold_weights = fold_weights)
  
  # Match observations to their group's fold
  folds <- group_folds[match(groups, group_levels)]

  # Return  --------------------------------------------------------------------
  return(folds)
  
}
