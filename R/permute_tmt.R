#' Permute a Treatment Assignment Vector
#' @export
#' 
#' @description Generate unique permutations of treatment assignment.
#' 
#' @inheritParams analyze
#' @param n_perm integer(1) \cr
#' The number of permutations to generate.
#' @param block vector \cr
#' Optional, observation-level block IDs.
#' @param cluster vector \cr
#' Optional, observation-level cluster IDs.
#' @param max_tries integer(1) \cr
#' The number of attempts to make at producing `n_perm` unique permutations.
#' After `max_tries`, the unique permutations generated so far will be returned.
#' 
#' @return [vector] Treatment assignments, permuted by cluster and within block.
#' Removes duplicates, returning only unique permutations
#' 
permute_tmt <- function(tmt,
                        n_perm  = 1L,
                        block   = NULL,
                        cluster = NULL,
                        max_tries = 10) {
  
  # Fast Returns ------------------------------------------------------------
  if (is.null(n_perm) || n_perm == 0) return(NULL)
  
  # Preprocess Arguments ----------------------------------------------------
  block   <- block   %||% rep(1, length(tmt))
  cluster <- cluster %||% seq_along(tmt)  
  
  # Defensive Checks --------------------------------------------------------
  # Should be just one treatment per cluster
  cluster_tmt <- tapply(tmt, cluster, unique)
  if (any(purrr::map_int(cluster_tmt, length) > 1)) {
    
    msg <- paste("All observations in a cluster must have the same treatment ",
                 "assignment. These clusters have multiple treatment levels: ",
                 commas(sort(unique(cluster))[purrr::map_int(cluster_tmt, length) > 1]))
    rlang::abort(message = msg)
  }
  
  # Function Body -----------------------------------------------------------
  # Get the block and treatment value for each cluster.
  # These are the first values of block and treatment per cluster index
  
  obs_cluster_index <- match(cluster, sort(unique(cluster)))
  cluster_block     <- tapply(block, obs_cluster_index, `[`, 1)
  cluster_tmt <- unlist(tapply(tmt, obs_cluster_index, `[`, 1, simplify = FALSE), use.names = FALSE)
  
  # Shuffle treatments within blocks
  permuted_tmt <- replicate(n_perm,
                            resample_treatment_clusterwise(cluster_tmt,
                                                           cluster_block, 
                                                           obs_cluster_index),
                            simplify = FALSE)
  
  # Make sure permutations are unique 
  if (anyDuplicated(permuted_tmt)) {
    
    permuted_tmt <- unique(permuted_tmt)
    unique_tries <- 1
    
    while (unique_tries <= max_tries & length(permuted_tmt) < n_perm) {
      
      unique_tries <- unique_tries + 1
      
      permuted_tmt <- c(permuted_tmt,
                        replicate(n_perm,
                                  resample_treatment_clusterwise(cluster_tmt,
                                                                 cluster_block, 
                                                                 obs_cluster_index),
                                  simplify = FALSE))
      
      permuted_tmt <- unique(permuted_tmt)
    }
    
    permuted_tmt <- utils::head(permuted_tmt, n_perm)
  }
  
  if (length(permuted_tmt) < n_perm) {
    msg <- paste("Only", length(permuted_tmt), "unique permutations of",
                 "treatment were generated after", max_tries, "attempts.",
                 "The requested", n_perm, "permutations may be",
                 "close to or greater than the maximum possible number",
                 "of permutations.")
    rlang::warn(message = msg)
  }
  
  # Match factor levels, if treatment is a factor
  if (is.factor(tmt)) {
    permuted_tmt <- lapply(permuted_tmt,
                           factor,
                           levels = levels(tmt))
  }
  
  return(permuted_tmt)
}


#' Permute Treatment Assignment at Cluster Level
#'
#' @param cluster_tmt Treatment assigned to each cluster.
#' @param cluster_block Block each cluster belongs to.
#' @param obs_cluster_index Cluster each observation belongs to.
resample_treatment_clusterwise <- function(cluster_tmt, cluster_block, obs_cluster_index) {
  
  # `unlist(lapply(split(x,y), fun))` is a faster version of `tapply(x, y, fun)`
  permuted_cluster_tmt <- unlist(lapply(split(cluster_tmt,
                                              cluster_block),
                                        sample),
                                 use.names = FALSE)
  
  # Grab the resampled treatment for each observation
  permuted_cluster_tmt[obs_cluster_index]
  
}
