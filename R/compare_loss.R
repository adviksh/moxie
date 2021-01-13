#" Compare Performances of Several Feature Sets
#" @param loss_tb data.frame \cr
#" A data.frame containing the observed and permuted losses earned by each feature_set
#" @param index_var character(1) \cr
#" Name of the grouping column in loss_tb.
compare_loss <- function(loss_tb) {
  
  # Fast Return -------------------------------------------------------------
  if (length(unique(loss_tb$feature_set)) == 1) return(NULL)
  
  # Function Body -----------------------------------------------------------
  # Construct a table with all comparisons of two feature sets
  compare_tb <- expand.grid(feature_set_a = unique(loss_tb$feature_set), 
                            feature_set_b = unique(loss_tb$feature_set),
                            loss_fun = unique(loss_tb$loss_fun),
                            stringsAsFactors = FALSE)
  compare_tb <- compare_tb[compare_tb$feature_set_a != 
                           compare_tb$feature_set_b, ]
  
  # Join the permuted losses
  compare_tb <- merge(compare_tb, loss_tb,
                      by.x = c("loss_fun", "feature_set_a"),
                      by.y = c("loss_fun", "feature_set"))
  compare_tb <- merge(compare_tb, loss_tb,
                      by.x = c("loss_fun", "feature_set_b"),
                      by.y = c("loss_fun", "feature_set"),
                      suffixes = c("_a", "_b"))
  
  # Calculate observed and permuted differences in model performance
  compare_tb$observed_a_minus_b <- purrr::map2_dbl(compare_tb$observed_loss_a,
                                                   compare_tb$observed_loss_b,
                                                   `-`)
  
  compare_tb$permuted_a_minus_b <- purrr::map2(compare_tb$permuted_loss_a,
                                               compare_tb$permuted_loss_b,
                                               `-`)
  
  compare_tb$p_val <- purrr::map2_dbl(compare_tb$observed_a_minus_b,
                                      compare_tb$permuted_a_minus_b,
                                      perm_p_val)
  
  # Return relevant columns
  compare_tb <- compare_tb[, c("feature_set_a", "feature_set_b",
                               "observed_loss_a", "observed_loss_b",
                               "observed_a_minus_b",
                               "permuted_a_minus_b",
                               "p_val")]
  
  return(compare_tb)
  
}