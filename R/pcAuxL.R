#' pcAuxL prints the GPL-3 that describe the licensing conditions for PcAux.
#'
#' @return License Information
#' @export pcAuxL

pcAuxL <- function()
  writeLines(readLines(system.file("LICENSE", package = "PcAux")))
