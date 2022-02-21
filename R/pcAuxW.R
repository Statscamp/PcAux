#' pcAuxW
#'
#' Displays the warranty information
#'
#' Displays the warranty information
#'
#' @return Warranty Information
#' @export pcAuxW

pcAuxW <- function() {
  lic <- readLines(system.file("LICENSE", package = "PcAux"))

  start <- grep("15. Disclaimer of Warranty", lic)
  end   <- grep("END OF TERMS AND CONDITIONS", lic) - 1

  writeLines(lic[start:end])
}
