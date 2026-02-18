.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  if (!exists(".obm_env", envir = ns, inherits = FALSE)) {
    assign(".obm_env", new.env(parent = emptyenv()), envir = ns)
  }
}
