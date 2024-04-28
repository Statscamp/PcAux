#' pcAuxW prints the sections of the GPL-3 that describe the warranty (or complete lack thereof) for PcAux.
#'
#' @return Text giving the warranty-specific sections of the GPL-3.
#' @export pcAuxW

pcAuxW <- function() {
  lic <- readLines(system.file("LICENSE", package = "PcAux"))

  start <- grep("15. Disclaimer of Warranty", lic)
  end   <- grep("END OF TERMS AND CONDITIONS", lic) - 1

  writeLines(lic[start:end])
}
