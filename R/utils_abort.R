abort_na <- function(what, where = NULL) {
  
  msg <- paste(what, "must not contain any NAs.")
  
  if (is_not_null(where)) {
    msg <- paste(msg, "It does at indices:", commas(where))
  }
  
  rlang::abort("error_na", message = msg)
}

abort_one_of <- function(what, one_of, not) {
  msg <- paste0(what, " must be one of: ", commas(one_of), "; not: ", not)
  
  rlang::abort("error_one_of", message = msg)
}