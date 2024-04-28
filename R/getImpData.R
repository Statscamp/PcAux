#' getImpData is a simple wrapper function that extracts the completed, multiply imputed data sets from a
#' fitted PcAuxData object produced by running the miWithPcAux function.
#'
#' @param pcAuxData A fitted object of class PcAuxData produced as output of the miWithPcAux func-
#' tion.
#' @return Imputed data sets. One data set for each nImps.
#' @export getImpData

getImpData <- function(pcAuxData) {
  pcAuxData$miDatasets
}
