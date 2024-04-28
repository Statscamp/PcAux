#' calcTime records the system time for the PcAux functions at set intervals to help determine is-
#' sues with lengthy runs. calcTime allows the user to extract timing information for specific functions
#' of PcAux individually.
#'
#' @param pcAuxData A fitted object of class PcAuxData produced as output of prepData, createPcAux,
#' or miWithPcAux functions.
#' @param what A character vector indicating the name of a function for which to extract status
#' information. abbreviates the functions prepData, createPcAux, and miWithPcAux
#' as "prep", "create", and "mi", respectively.
#' @return A named vector with an entry per interval.
#' @export calcTime

calcTime <- function(pcAuxData, what) {
  time     <- pcAuxData$time[[what]]
  eachStep <- diff(time)

  nPoints                      <- length(eachStep)
  eachStep[nPoints + 1]        <-
    as.vector(time[length(time)] - time["start"])
  names(eachStep)[nPoints + 1] <- "overall"

  usrVars <-
    lapply(c("End", "usr"), function(x)
      grep(x, names(eachStep)))

  if (length(unlist(usrVars)) > 1) {
    eachStep["overall"] <- eachStep["overall"] - eachStep[usrVars[[1]]]
    timeSteps           <- eachStep[-usrVars[[2]]]
    timeSteps["usr"]    <- eachStep[usrVars[[1]]]
  }
  else
    timeSteps <- eachStep

  timeSteps
}
