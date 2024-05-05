#' Extract principal component scores for use as the auxiliary variables in a
#' principled missing data analysis as described by Howard, Rhemtulla, and
#' Little (2015).
#'
#' @param pcAuxData An object of class PcAuxData returned from prepData.
#' @param nComps A two-element, numeric vector giving the number of linear and
#'   nonlinear, re- spectively, component scores to extract. See the Details
#'   section for more infor- mation.
#' @param interactType An integer code indicating which method is used to
#'   incorporate interactions into the initial, single imputation model. See the
#'   Details section for more information. Defaults to interactType = 1L.
#' @param maxPolyPow An integer giving the maximum power used when constructing
#'   the polynomial terms. Setting maxPolyPow = 1L has the effect of excluding
#'   any polynomial terms from the imputation model. Defaults to maxPolyPow =
#'   3L.
#' @param simMode A logical switch turning ’Simulation Mode’ on or off. In
#'   Simulation Mode all of the automatic data checks will be suppressed. This
#'   mode is intended for use when pcAux is being called as part of a Monte
#'   Carlo simulation study in which the data properties are well-known by the
#'   user. This mode should not be used for ’real-world’ data analysis. Defaults
#'   to simMode = FALSE.
#' @param seed An optional integer used to seed the random number generator used
#'   by the im- putation algorithm. Defaults to seed = NULL which leaves the
#'   default random number generator unaltered.
#' @param verbose An integer code in 0, 1, 2 deﬁning the verbosity of output
#'   printed to the screen. verbose = 0prints no output; verbose = 1prints all
#'   output except for the mes- sages returned by mice; verbose = 2 prints all
#'   output, including the messages returned by mice. Warnings are always
#'   printed, regardless of the value assigned to verbose. Defaults to verbose =
#'   2.
#' @param doImputation A logical switch indicating whether the data should be
#'   imputed before extracting the principal component scores. Set to FALSE if
#'   the data element in pcAuxData has no missing values (e.g., the imputation
#'   was done elsewhere). Defaults to doImputation = TRUE.
#' @param castData A logical switch indicating whether the data element in
#'   pcAuxData should have its variables re-typed. Keep as FALSE unless the data
#'   have been manipulated after running prepData. Defaults to castData = FALSE.
#' @param control An optional list of control parameters (see ’Details’).
#' @param micemethods A list of mice methods to use for imputation.
#' @param ... Currently unused
#'
#' @return An Reference Class object of class PcAuxData with ﬁelds for each of
#'   the createPcAux function’s arguments (except for the raw data which are
#'   removed to save resources).
#' @export

createPcAux <- function(pcAuxData,
                        nComps,
                        interactType = 0L,
                        maxPolyPow   = 1L,
                        simMode      = FALSE,
                        seed         = NULL,
                        verbose      = 2L,
                        doImputation = TRUE,
                        castData     = !doImputation,
                        control,
                        micemethods = c("norm", "polr", "polyreg", "logreg"),
                        ...)
{
    pcAuxData$setCall(match.call(), parent = "createPcAux")

    ## Set initial time and status check
    pcAuxData$setTime()
    if(pcAuxData$checkStatus == "start" | pcAuxData$checkStatus == "all")
        pcAuxData$setStatus()


    ## Check for problems with the input values:
    if(missing(pcAuxData)) errFun("noPcAuxData")
    if(missing(nComps))    errFun("noNComps")
    if(!simMode)           checkInputs()

    ## Add elements to an extant instance of the PcAuxData class:
    pcAuxData$nComps   <- nComps
    pcAuxData$forcePmm <- FALSE # Don't give imputation options other than PMM
    pcAuxData$intMeth  <- as.integer(interactType)
    pcAuxData$maxPower <- as.integer(maxPolyPow)
    pcAuxData$simMode  <- simMode
    pcAuxData$verbose  <- as.integer(verbose)

    if(!missCheck(seed)) pcAuxData$seed <- as.integer(seed)

    ## Make sure the control list is fully populated:
    if(!missCheck(control)) pcAuxData$setControl(x = control)

    pcAuxData$setTime("dataCheck")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("dataCheck")

    ## Check for extant moderators:
    check <- missCheck(pcAuxData$moderators)
    if(check) {
        pcAuxData$moderators <- colnames(pcAuxData$data)
        if(pcAuxData$intMeth ==1 | pcAuxData$intMeth == 2)
            warnFun("noMods", map = pcAuxData)
    }

    pcAuxData$setTime("modExt")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("modExt")

    ## Re-cast the data if needed
    if(castData | interactType == 1) castData(map = pcAuxData)

    pcAuxData$setTime("cast")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("cast")

    if(doImputation) {
        ## Check for and treat any nominal variables that are missing only one
        ## datum:
        singleMissNom <-
            with(pcAuxData, (nrow(data) - respCounts == 1) &
                            (typeVec == "binary" | typeVec == "nominal")
                 )
        ## KML 2016-NOV-14: Ignore dropped variables
        singleMissNom <- setdiff(names(singleMissNom)[singleMissNom],
                                 pcAuxData$dropVars[ , 1])

        if(length(singleMissNom) > 0) pcAuxData$fillNomCell(singleMissNom)

        pcAuxData$setTime("doImp")
        if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("doImp")
    }

    ## Compute interactions for use during initial imputation:
    if(pcAuxData$intMeth == 1) {
        pcAuxData$computeInteract()
        pcAuxData$data     <- with(pcAuxData, data.frame(data, interact))
        pcAuxData$interact <- "Removed to save resources"
    }

    pcAuxData$setTime("compInt")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("compInt")

    ## Compute polynomials for use during initial imputation:
    if(pcAuxData$maxPower > 1) {
        pcAuxData$computePoly()
        pcAuxData$data <- with(pcAuxData, data.frame(data, poly))
        if(pcAuxData$intMeth == 1) pcAuxData$poly <- "Removed to save resources"
    }

    pcAuxData$setTime("compPoly")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("compPoly")

    ## Execute the initial, single imputation:
    if(doImputation) {

        doSingleImputation(map = pcAuxData, micemethods = micemethods)

        ## Use imputed data to update nominal variable representations:
        if(length(pcAuxData$nomVars) > 0) pcAuxData$codeNomVars()

        pcAuxData$setTime("doSingle")
        if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("doSingle")
    }

    ## Extract the linear principal component scores:
    doPCA(map = pcAuxData)

    pcAuxData$setTime("doPCA")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("doPCA")

    ## Are we constructing seperate non-linear PcAux?
    if(pcAuxData$nComps[2] != 0) {
        ## Undo dummy coding to facilitate interaction calculation:
        if(!missCheck(pcAuxData$nomVars))
            pcAuxData$castNomVars(toNumeric = FALSE)

        ## Construct and orthogonalize interaction terms:
        if(pcAuxData$intMeth > 1) pcAuxData$computeInteract()

      write.csv(pcAuxData$data, file = "C:\Users\Danny Squire\Documents\Projects\data.csv")
      write.csv(pcAuxData$interact, file = "C:\Users\Danny Squire\Documents\Projects\interact.csv")

        ## Extract the nonlinear principal component scores:
        doPCA(map = pcAuxData)

        pcAuxData$setTime("doNLinear")
        if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("doNLinear")
    }

    ## Remove unnecessary representation of nominal variables:
    pcAuxData$facNoms <- "Removed to save resources"
    pcAuxData$dumNoms <- "Removed to save resources"

    pcAuxData$setTime("rmVars")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("rmVars")

    pcAuxData
}# END createPcAux()
