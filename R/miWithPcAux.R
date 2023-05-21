#' Create multiple imputations with the mice package using the principal
#' component auxiliary variable scores produced by createPcAux as the predictors
#' in the imputation model.
#'
#' @param rawData A data frame containing the incomplete data for which to
#'   create the multiple imputations.
#' @param pcAuxData An object of class PcAuxData produced by a run of
#'   createPcAux.
#' @param nImps An integer giving the number of imputations to create. Defaults
#'   to nImps = 100L.
#' @param nomVars An optional character vector containing names of any nominal
#'   variables (i.e., unordered factors) that exist in rawData. If unspeciﬁed,
#'   any nomVarsdeﬁned in pcAuxDatawill be used.
#' @param ordVars An optional character vector containing names of any ordinal
#'   variables (i.e., ordered factors) that exist in rawData. If unspeciﬁed, any
#'   ordVars deﬁned in pcAuxDatawill be used.
#' @param idVars An optional character vector containing names of any ID
#'   variables that exist in rawData. Any columns ﬂagged as ID variables should
#'   not be represented in nomVars, ordVars, dropVars, or groupVars. If
#'   unspeciﬁed, any idVars deﬁned in pcAuxDatawill be used.
#' @param dropVars An optional character vector containing names of any nuisance
#'   variables that should be excluded from the imputation process. If
#'   unspeciﬁed, the default value of dropVars = "useExtant" causes any
#'   user-deﬁned dropVars deﬁned in pcAuxData to be used.
#' @param nComps A two-element vector giving the number of linear and nonlinear,
#'   respectively, component scores to extract. See the Details section for more
#'   information. When not speciﬁed, all component scores that exist in
#'   pcAuxData are used.
#' @param compFormat The format in which the multiply-imputed data sets are
#'   returned. Valid argu- ments are "list", which returns a list of length
#'   nImps with each entry containing one imputed data set, "long", "broad", and
#'   "repeated". The latter three options are passed directly to the action
#'   argument of the mice::complete function. See the documentation for
#'   mice::completefor more details on the behavior of the "long", "broad", and
#'   "repeated" options. Defaults to compFormat = "list".
#' @param seed An optional integer used to seed the random number generator used
#'   by the im- putation algorithm. Defaults to seed = NULL which employs any
#'   seed deﬁned in createPcAux and, otherwise, leaves the default random number
#'   generator unaltered.
#' @param simMode A logical switch turning ’Simulation Mode’ on or off. In
#'   Simulation Mode all of the automatic data checks will be suppressed. This
#'   mode is intended for use when miWithPcAuxis being called as part of a Monte
#'   Carlo simulation study in which the data properties are well-known by the
#'   user. This mode should not be used for ’real-world’ data analysis. Defaults
#'   to simMode = FALSE.
#' @param forcePmm A logical ﬂag indicating whether or not the imputation should
#'   use predictive mean matching as the elementary imputation method for
#'   (almost) all variables. If forcePmm == FALSE, the elementary imputation
#'   methods are chosen to match each variable’s declared type. When forcePmm ==
#'   TRUE, nominal variables are still imputed with GLM-based methods
#'   appropriate for their declared types, but all other variables are imputed
#'   with PMM. Defaults to forcePmm = FALSE.
#' @param nProcess An integer that gives the number of parallel processes to use
#'   when for parallel MI. Must be less than or equal to the number of available
#'   logical processor cores. A value of nProcess = 1L results in serial MI
#'   processing. Defaults to nProcess = 1L.
#' @param verbose An integer code in 0, 1, 2 deﬁning the verbosity of output
#'   printed to the screen. verbose = 0prints no output; verbose = 1prints all
#'   output except for the mes- sages returned by mice; verbose = 2 prints all
#'   output, including the messages returned by mice. Warnings are always
#'   printed, regardless of the value assigned to verbose. Defaults to verbose =
#'   2.
#' @param control An optional list of control parameters (see ’Details’).
#' @param micemethods Which `mice` imputation method to use.
#'
#' @return A Reference Class object of class PcAuxData.
#' @export

miWithPcAux <- function(rawData,
                        pcAuxData,
                        nImps      = 100L,
                        nomVars    = NULL,
                        ordVars    = NULL,
                        idVars     = NULL,
                        dropVars   = "useExtant",
                        nComps     = NULL,
                        compFormat = "list",
                        seed       = NULL,
                        simMode    = FALSE,
                        forcePmm   = FALSE,
                        nProcess   = 1L,
                        verbose    = 2L,
                        control,
                        micemethods = c("norm", "polr", "polyreg", "logreg")
                       )
{
    pcAuxData$setCall(match.call(), parent = "miWithPcAux")

    pcAuxData$setTime()
    if(pcAuxData$checkStatus == "start" | pcAuxData$checkStatus == "all") pcAuxData$setStatus()

    if(missing(rawData))   errFun("noData")
    if(missing(pcAuxData)) errFun("noPcAuxData")

    ## Get variable types:
    if(!missCheck(nomVars)) {
        pcAuxData$nomVars  <- nomVars
        removeNoms         <- which(pcAuxData$dropVars[,1] %in% nomVars)
        pcAuxData$dropVars <- pcAuxData$dropVars[-removeNoms, ]
    }
    if(!missCheck(ordVars)) {
        pcAuxData$ordVars  <- ordVars
        removeOrds         <- which(pcAuxData$dropVars[,1] %in% ordVars)
        pcAuxData$dropVars <- pcAuxData$dropVars[-removeOrds, ]
    }
    if(!missCheck(idVars)) {
        pcAuxData$idVars   <- idVars
        removeIds          <- which(pcAuxData$dropVars[,1] %in% idVars)
        pcAuxData$dropVars <- pcAuxData$dropVars[-removeIds, ]
    }
    if(length(dropVars) == 1 && dropVars == "useExtant") {
        tmp <- pcAuxData$dropVars[pcAuxData$dropVars[ , 2] == "user_defined", ]
        if(!is.matrix(tmp)) tmp <- matrix(tmp, 1, 2)
        pcAuxData$dropVars <- tmp
    } else if(!missCheck(dropVars)) {
        pcAuxData$dropVars <- cbind(dropVars, "user_defined")
        pcAuxData$nomVars  <- setdiff(pcAuxData$nomVars, dropVars)
        pcAuxData$ordVars  <- setdiff(pcAuxData$ordVars, dropVars)
        pcAuxData$idVars   <- setdiff(pcAuxData$idVars, dropVars)
    } else {
        pcAuxData$dropVars <- cbind("NONE_DEFINED", "user_defined")
    }

    ## Check inputs' validity:
    if(!simMode) checkInputs()

    pcAuxData$setTime("varTypes")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("varTypes")

    ## Combine the principal component auxiliaries with the raw data:
    mergePcAux(pcAuxData = pcAuxData,
               rawData   = rawData,
               nComps    = nComps,
               intern    = TRUE)

    pcAuxData$setTime("mergePcAux")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("mergePcAux")

    ## Populate new fields in the extant PcAuxData object:
    pcAuxData$nImps      <- as.integer(nImps)
    pcAuxData$simMode    <- simMode
    pcAuxData$compFormat <- compFormat
    pcAuxData$forcePmm   <- forcePmm
    pcAuxData$verbose    <- as.integer(verbose)

    if(!missCheck(seed)) pcAuxData$seed <- as.integer(seed)

    ## Make sure the control list is fully populated:
    if(!missCheck(control)) pcAuxData$setControl(x = control)

    pcAuxData$setTime("popNew")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("popNew")

    ## Cast the variables to their appropriate types:
    castData(map = pcAuxData)

    ## Check and clean the data:
    if(!simMode) cleanData(map = pcAuxData)

    pcAuxData$setTime("reCast")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("reCast")

    ## Check for and treat any single nominal variables that are missing
    ## only one datum
    singleMissNom <- with(pcAuxData, (nrow(data) - respCounts == 1) &
                                     (typeVec == "binary" | typeVec == "nominal")
                          )
    if(any(singleMissNom))
        pcAuxData$fillNomCell(colnames(pcAuxData$data[singleMissNom]))

    pcAuxData$setTime("nomMis")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("nomMis")

    if(verbose > 1) cat("\nMultiply imputing missing data...\n")

    ## Construct a predictor matrix for mice():
    if(verbose > 1) {
      cat("--Constructing predictor matrix...",
          "\n MinPcPredCOr =", pcAuxData$minPcPredCor,
          "\n MinPcPredCount =", pcAuxData$minPcPredCount,
          "\n")
    }
    nLin    <- length(grep("^linPC\\d", colnames(pcAuxData$data)))
    nNonLin <- ifelse(nComps[2] == 0,
                      0,
                      length(grep("^nonLinPC\\d", colnames(pcAuxData$data)))
                      )

    pcAuxData$predMat <- pcQuickPred(data   = pcAuxData$data,
                           nLinear      = nLin,
                           nNonLinear   = nNonLin,
                           mincor       = pcAuxData$minPcPredCor,
                           minPredCount = pcAuxData$minPcPredCount
                              )
    if(verbose > 1) cat("done.\n")

    pcAuxData$setTime("predMat")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("predMat")

    ## Specify a vector of elementary imputation methods:
    if(verbose > 1) cat("--Creating method vector...")
    pcAuxData$createMethVec(micemethods = micemethods)
    if(verbose > 1) cat("done.\n")

    pcAuxData$setTime("methVec")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("methVec")

    if(forcePmm & verbose > 1) cat("PMM forced by user.\n")

    ## Multiply impute the missing data in 'mergedData':
    if(verbose > 1) cat("--Imputing missing values...\n")

    if(nProcess == 1) {# Impute in serial
        pcAuxData$miceObject <- try(
            mice(pcAuxData$data,
                 m               = nImps,
                 maxit           = 1L,
                 predictorMatrix = pcAuxData$predMat,
                 method          = pcAuxData$methVec,
                 printFlag       = verbose == 2,
                 ridge           = pcAuxData$miceRidge,
                 seed            = pcAuxData$seed,
                 nnet.MaxNWts    = pcAuxData$maxNetWts),
            silent = TRUE)

        pcAuxData$setTime("impSerial")
        if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("impSerial")

        # We are making the assumption that if mice crashes a mids object won't
        # be created.
        #if(class(pcAuxData$miceObject) != "try-error") {
        if(isa(pcAuxData$miceObject, "mids")) {
            pcAuxData$data <- "Removed to save resources."

            ## Complete the incomplete data sets:
            pcAuxData$completeMiData()

            pcAuxData$setTime("completeMi")
            if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("completeMi")

        } else {
            errFun("miceCrash", map = pcAuxData)
        }

    } else {# Impute in parallel
        myCluster <- makeCluster(nProcess)
        clusterEvalQ(myCluster, library(mice))

        pcAuxData$miDatasets <- parLapply(myCluster,
                                          X       = c(1 : nImps),
                                          fun     = parallelMice,
                                          map     = pcAuxData,
                                          tempDirName = tempdir())

        stopCluster(myCluster)

        pcAuxData$setTime("impParallel")
        if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("impParallel")

        # assemble list of returned datasets into single miDataset object
        pcAuxData$transformMiData()

        # retrieve mids object for 1st parallel process and put in PcAuxObject
        pcAuxData$miceObject <- readRDS(file = file.path(tempdir(),"firstMids.RDS"))

        pcAuxData$setTime("transformMI")
        if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("transformMI")

    }

    if(verbose > 1) cat("\n--done.\n")
    if(verbose > 1) cat("Complete.\n")

    pcAuxData$setTime("miEnd")
    if(pcAuxData$checkStatus == "all") pcAuxData$setStatus("miEnd")

    pcAuxData
}# END miWithPcAux()

