#' getLoggedEvents retrieves logged events from the imputation run
#'
#' Returns Logged Events
#'
#' Returns Logged Events
#'
#' @param pcAuxData pcAuxData object
#' @return Logged Events
#' @export getLoggedEvents
getLoggedEvents <- function(pcAuxData) {
  if (!nrow(pcAuxData$loggedEvents))
    return("No logged events")
  else
    return(pcAuxData$loggedEvents)
}
