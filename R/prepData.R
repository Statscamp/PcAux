#' Prepare Data for Extracting Principal Component Auxiliaries
#'
#' @param rawData A data frame from which to extract the auxiliary principal
#'   components.
#' @param moderators An optional character vector containing names of any
#'   moderator variables to in- clude in the initial, single imputation model.
#'   The variables supplied here will be interacted with all other observed
#'   variables when specifying the initial single im- putation model’s
#'   systematic component. The exact method by which this mod- eration is
#'   incorporated depends on the interactTypeargument in createPcAux (see the
#'   documentation for createPcAux for more information).
#' @param nomVars An optional character vector containing names of any nominal
#'   variables (i.e., unordered factors) that exist in rawData.
#' @param ordVars An optional character vector containing names of any ordinal
#'   variables (i.e., ordered factors) that exist in rawData.
#' @param idVars An optional character vector containing names of any ID
#'   variables that exist in rawData. Any columns ﬂagged as ID variables should
#'   not be represented in nomVars, ordVars, dropVars, or groupVars
#' @param dropVars An optional character vector containing names of any nuisance
#'   variables that should be dropped before extracting the auxiliary principal
#'   component scores.
#' @param groupVars An optional character vector containing names of any
#'   grouping variables that can be used to create the strata that deﬁne the
#'   groups used by the fall-back group-mean substitution. If continuous
#'   grouping variables are speciﬁed, they are binned via the cut()function with
#'   breaks = control$nGVarCuts.
#' @param simMode A logical switch turning ’Simulation Mode’ on or off. In
#'   Simulation Mode all of the automatic data checks will be suppressed. This
#'   mode is intended for use when prepData is being called as part of a Monte
#'   Carlo simulation study in which the data properties are well-known by the
#'   user. This mode should not be used for ’real-world’ data analysis. Defaults
#'   to simMode = FALSE.
#' @param nProcess An integer indicating the number of processors to use when
#'   using parallel pro- cessing for the collinearity checks. A value of
#'   nProcess = 1L results in serial processing. Must be less than or equal to
#'   the available number of logical pro- cessing cores. Defaults to nProcess =
#'   1L.
#' @param verbose An integer code in 0, 1, 2 deﬁning the verbosity of output
#'   printed to the screen. verbose = 0prints no output; verbose = 1prints all
#'   output except for the mes- sages returned by mice; verbose = 2 prints all
#'   output, including the messages returned by mice. Warnings are always
#'   printed, regardless of the value assigned to verbose. Defaults to verbose =
#'   2.
#' @param control An optional list of control parameters (see ’Details’).
#' @param ... Not currently used.
#'
#' @return Reference Class object of class PcAuxData
#' @export

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
    if(missing(rawData)) errFun("noData")
    if(!simMode)         checkInputs()

    if(missCheck(dropVars)) dropVars <- "NONE_DEFINED"

    ## Initialize a new instance of the PcAuxData class
    ## to store all of the data and metadata for this run:
    pcAuxData <- PcAuxData(data     = rawData,
                           dropVars = dropVars,
                           simMode  = simMode,
                           nProcess = as.integer(nProcess),
                           verbose  = as.integer(verbose)
                           )

    pcAuxData$setCall(match.call(), parent = "prepData")
    pcAuxData$setTime()

    ## Make sure the control list is fully populated:
    if(!missCheck(control)) pcAuxData$setControl(x = control)

    ## Set initial machine check
    if(pcAuxData$checkStatus == "start" | pcAuxData$checkStatus == "all") pcAuxData$setStatus()

    ## Check for special variable arguments and fill the appropriate slots in the
    ## pcAuxData object:
    if(!missCheck(idVars)) {
        pcAuxData$idVars <- idVars
        pcAuxData$idCols <- as.data.frame(pcAuxData$data[ , idVars])
        pcAuxData$data   <-
            pcAuxData$data[ , setdiff(colnames(pcAuxData$data), idVars)]
    }
    if(!missCheck(groupVars))  pcAuxData$groupVars  <- groupVars
    if(!missCheck(nomVars))    pcAuxData$nomVars    <- nomVars
    if(!missCheck(ordVars))    pcAuxData$ordVars    <- ordVars
    if(!missCheck(moderators)) pcAuxData$moderators <- moderators

    pcAuxData$setTime("checkSpecial")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("checkSpecial")

### Pre-process the data ###

    ## Cast the variables to their declared types:
    castData(map = pcAuxData)

    if(!simMode) {# Don't clean data in simMode
        ## Remove constant and empty columns:
        cleanData(map = pcAuxData)

        ## Find any (bivariate) collinear variables:
        findCollin(map = pcAuxData)
    }

    pcAuxData$setTime("cast")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("cast")

    pcAuxData
}# END prepData()
