#' writeStatus provides a text file featuring system and OS information for documentation purposes.
#'
#' @param pcAuxData A fitted object of class PcAuxData produced as output of prepData, createPcAux,
#' or miWithPcAux functions.
#' @param outName A character vector indicating the name of the file to write the information to,
#' such as a .txt file.
#' @param what A character vector indicating the name of a function for which to extract status
#' information. abbreviates the functions prepData, createPcAux, and miWithPcAux
#' as "prep", "create", and "mi", respectively.
#' @return A string object containing the compiled system and OS information.
#' @export writeStatus

writeStatus <- function(pcAuxData, outName, what) {
  utils::capture.output(pcAuxData$status[[what]], file = outName)
  paste("Wrote status to", outName)
}
