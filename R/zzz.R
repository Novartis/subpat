# Write a warning when package is loaded.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(pkgname, "is NOT VALIDATED, and should be reserved for exploratory analysis only."))
}