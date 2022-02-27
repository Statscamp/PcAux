#' writeStatus writes status of imputation run to log file
#'
#' Returns status
#'
#' Returns status
#'
#' @param pcAuxData pcAuxData object
#' @param outName name of the output file
#' @param what the status you want to know
#' @return status
#' @export writeStatus
writeStatus <- function(pcAuxData, outName, what) {
  utils::capture.output(pcAuxData$status[[what]], file = outName)
  paste("Wrote status to", outName)
}
