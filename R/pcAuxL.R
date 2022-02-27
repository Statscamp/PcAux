#' pcAuxL
#'
#' Displays the license information
#'
#' Displays the license information
#'
#' @return License Information
#' @export pcAuxL
pcAuxL <- function()
  writeLines(readLines(system.file("LICENSE", package = "PcAux")))
