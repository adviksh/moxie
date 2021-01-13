#' Rowbind and Index a List of Tables
#' @description Rowbinds a list of tables and creates a new indexing column
#' to identify which table each row in the result came from.
#' 
#' @param list_of_tb list \cr
#' A list of data.frames
#' @param list_names character \cr
#' A character vector naming each table in list_of_tb
#' @param new_column character(1) \cr
#' Name of the index column to create in the result. Will hold entries from
#' list_names.
#' 
#' @return data.frame
combine_tbs <- function(list_of_tb, list_names, new_column) {
  
  # Function Body -----------------------------------------------------------
  list_names <- list_names[purrr::map_lgl(list_of_tb, is_not_null)]
  list_of_tb <- list_of_tb[purrr::map_lgl(list_of_tb, is_not_null)]
  
  if (length(list_of_tb) == 0) { return(NULL) }
  if (length(list_of_tb) == 1) { return(list_of_tb[[1]])  }
  
  combined <- purrr::map2_df(list_of_tb, list_names,
                             function(x, y, new_c) {
                               x[[new_column]] <- y
                               return(x)
                             },
                             new_c = new_column)
  
  new_order <- c(new_column, setdiff(colnames(combined), new_column))
  
  combined[, new_order]
}

#' Calculate Permutation P-Value
#' 
#' @param observed numeric \cr
#' Observed loss.
#' @param permuted numeric \cr
#' Permutation losses.
perm_p_val <- function(observed, permuted) {
  
  # Fast Returns ------------------------------------------------------------
  if (is.null(permuted)) return(NULL)
  
  # Function Body -------------------------------------------------------------
  mean(permuted <= observed)
}

is_not_null <- function(x) { !rlang::is_null(x) }

say <- function(verbose, ...) { if (verbose) message(...) }

`%||%` <- function(x, y) { if (rlang::is_null(x)) y else x }

arg_vec <- function(fn) {
  
  if (is.null(fn)) return(NULL)
  
  names(formals(fn))
}

commas <- function(x) { paste(x, collapse = ", ") }

is_proportion <- function(x) { rlang::is_scalar_double(x) & x >= 0 & x <= 1 }

is_lengthy_integerish <- function(x) { length(x) > 1 & rlang::is_integerish(x) }
