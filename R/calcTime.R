#' calcTime computes elapsed time at each interval
#'
#' Returns elapsed time for each imputation step
#'
#' Returns elapsed time for each imputation step
#'
#' @param pcAuxData pcAuxData object
#' @param what ???
#' @return Elapsed time for each imputation step
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
