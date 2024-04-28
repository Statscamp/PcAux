#' getLoggedEvents retrieves `mice' logged events from the imputation run.
#'
#' @param pcAuxData pcAuxData object
#' @return Logged events from `mice`
#' @export getLoggedEvents

getLoggedEvents <- function(pcAuxData) {
  if (!nrow(pcAuxData$loggedEvents))
    return("No logged events")
  else
    return(pcAuxData$loggedEvents)
}
