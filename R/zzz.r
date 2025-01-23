.undid_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .populate_undid_env(.undid_env)
}