#' Get or set OpenBioMaps session environment
#' @keywords internal
obm_env <- function() .obm_env

#' Assign a variable into the OBM session environment
#' @keywords internal
set_obm <- function(name, value) {
  assign(name, value, envir = obm_env())
}

#' Retrieve a variable from the OBM session environment
#' @keywords internal
get_obm <- function(name, default = NULL) {
  if (!exists(name, envir = obm_env(), inherits = FALSE)) {
    return(default)
  }
  get(name, envir = obm_env(), inherits = FALSE)
}
