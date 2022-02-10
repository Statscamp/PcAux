#' Prepare a data frame for conversion into a PcAuxData object
#'
#' `prepData` checks the input data and returns a PcAuxData object.
#'
#' This is the first step in generating auxillary variables.
#'
#' @param rawData A data frame or matrix containing raw data
#' @param moderators moderators
#' @param nomVars A list of column names which contain nominal values
#' @param ordVars A list of column names which contain ordinal values
#' @param idVars A list of column names which contain unique row
#' identifiers
#' @param dropVars A list of column names to exclude from the
#' imputation
#' @param groupVars A list of column names which contains group
#' @param simMode simMode
#' @param nProcess nProcess
#' @param verbose verbose
#' @param control control
#' @param ... other
#' @return An S4 class of type pcAuxData as defined in 01_PcAuxData.R
#' @export prepData
prepData <- function(rawData,
                     moderators = NULL,
                     nomVars    = NULL,
                     ordVars    = NULL,
                     idVars     = NULL,
                     dropVars   = NULL,
                     groupVars  = NULL,
                     simMode    = FALSE,
                     nProcess   = 1L,
                     verbose    = 2L,
                     control,
                     ...)
{
  ## Check for problems with the input values:
  if (missing(rawData))
    errFun("noData")
  if (!simMode)
    checkInputs()

  if (missCheck(dropVars))
    dropVars <- "NONE_DEFINED"

  ## Initialize a new instance of the PcAuxData class
  ## to store all of the data and metadata for this run:
  pcAuxData <- PcAuxData(
    data     = rawData,
    dropVars = dropVars,
    simMode  = simMode,
    nProcess = as.integer(nProcess),
    verbose  = as.integer(verbose)
  )

  pcAuxData$setCall(match.call(), parent = "prepData")
  pcAuxData$setTime()

  ## Make sure the control list is fully populated:
  if (!missCheck(control))
    pcAuxData$setControl(x = control)

  ## Set initial machine check
  if (pcAuxData$checkStatus == "start" |
      pcAuxData$checkStatus == "all")
    pcAuxData$setStatus()

  ## Check for special variable arguments and fill the appropriate slots in the
  ## pcAuxData object:
  if (!missCheck(idVars)) {
    pcAuxData$idVars <- idVars
    pcAuxData$idCols <- as.data.frame(pcAuxData$data[, idVars])
    pcAuxData$data   <-
      pcAuxData$data[, setdiff(colnames(pcAuxData$data), idVars)]
  }
  if (!missCheck(groupVars))
    pcAuxData$groupVars  <- groupVars
  if (!missCheck(nomVars))
    pcAuxData$nomVars    <- nomVars
  if (!missCheck(ordVars))
    pcAuxData$ordVars    <- ordVars
  if (!missCheck(moderators))
    pcAuxData$moderators <- moderators

  pcAuxData$setTime("checkSpecial")
  if (pcAuxData$checkStatus == "all")
    pcAuxData$setStatus("checkSpecial")

  ### Pre-process the data ###

  ## Cast the variables to their declared types:
  castData(map = pcAuxData)

  if (!simMode) {
    # Don't clean data in simMode
    ## Remove constant and empty columns:
    cleanData(map = pcAuxData)

    ## Find any (bivariate) collinear variables:
    findCollin(map = pcAuxData)
  }

  pcAuxData$setTime("cast")
  if (pcAuxData$checkStatus == "all")
    pcAuxData$setStatus("cast")

  pcAuxData
}# END prepData()
