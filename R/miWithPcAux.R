#' A collection of helper functions
#'
#' Various helper functions used throughout the package
#'
#' Various helper functions used throughout the package
#'
#' @param rawData Raw data
#' @param pcAuxData pcAuxData object
#' @param nImps Number of imputations to perform
#' @param nomVars Nominal Variables
#' @param ordVars Ordinal Variables
#' @param idVars ID Variables
#' @param dropVars Variables dropped from imputation
#' @param nComps ???
#' @param compFormat ???
#' @param seed Random seed for repeatable results
#' @param simMode Use simulation mode
#' @param forcePmm Force use of Pmm imputation method
#' @param nProcess Number of processors to use
#' @param verbose Use verbose mode
#' @param control ???
#' @param micemethods mice::mice() paramenters
#' @export miWithPcAux

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
                        micemethods = c("norm", "polr", "polyreg", "logreg"))
{
  pcAuxData$setCall(match.call(), parent = "miWithPcAux")

  pcAuxData$setTime()
  if (pcAuxData$checkStatus == "start" |
      pcAuxData$checkStatus == "all")
    pcAuxData$setStatus()

  if (missing(rawData))
    errFun("noData")
  if (missing(pcAuxData))
    errFun("noPcAuxData")

  ## Get variable types:
  if (!missCheck(nomVars)) {
    pcAuxData$nomVars  <- nomVars
    removeNoms         <- which(pcAuxData$dropVars[, 1] %in% nomVars)
    pcAuxData$dropVars <- pcAuxData$dropVars[-removeNoms,]
  }
  if (!missCheck(ordVars)) {
    pcAuxData$ordVars  <- ordVars
    removeOrds         <- which(pcAuxData$dropVars[, 1] %in% ordVars)
    pcAuxData$dropVars <- pcAuxData$dropVars[-removeOrds,]
  }
  if (!missCheck(idVars)) {
    pcAuxData$idVars   <- idVars
    removeIds          <- which(pcAuxData$dropVars[, 1] %in% idVars)
    pcAuxData$dropVars <- pcAuxData$dropVars[-removeIds,]
  }
  if (length(dropVars) == 1 && dropVars == "useExtant") {
    tmp <-
      pcAuxData$dropVars[pcAuxData$dropVars[, 2] == "user_defined",]
    if (class(tmp) != "matrix")
      tmp <- matrix(tmp, 1, 2)
    pcAuxData$dropVars <- tmp
  } else if (!missCheck(dropVars)) {
    pcAuxData$dropVars <- cbind(dropVars, "user_defined")
    pcAuxData$nomVars  <- setdiff(pcAuxData$nomVars, dropVars)
    pcAuxData$ordVars  <- setdiff(pcAuxData$ordVars, dropVars)
    pcAuxData$idVars   <- setdiff(pcAuxData$idVars, dropVars)
  } else {
    pcAuxData$dropVars <- cbind("NONE_DEFINED", "user_defined")
  }

  ## Check inputs' validity:
  if (!simMode)
    checkInputs()

  pcAuxData$setTime("varTypes")
  if (pcAuxData$checkStatus == "all")
    pcAuxData$setStatus("varTypes")

  ## Combine the principal component auxiliaries with the raw data:
  mergePcAux(
    pcAuxData = pcAuxData,
    rawData   = rawData,
    nComps    = nComps,
    intern    = TRUE
  )

  pcAuxData$setTime("mergePcAux")
  if (pcAuxData$checkStatus == "all")
    pcAuxData$setStatus("mergePcAux")

  ## Populate new fields in the extant PcAuxData object:
  pcAuxData$nImps      <- as.integer(nImps)
  pcAuxData$simMode    <- simMode
  pcAuxData$compFormat <- compFormat
  pcAuxData$forcePmm   <- forcePmm
  pcAuxData$verbose    <- as.integer(verbose)

  if (!missCheck(seed))
    pcAuxData$seed <- as.integer(seed)

  ## Make sure the control list is fully populated:
  if (!missCheck(control))
    pcAuxData$setControl(x = control)

  pcAuxData$setTime("popNew")
  if (pcAuxData$checkStatus == "all")
    pcAuxData$setStatus("popNew")

  ## Cast the variables to their appropriate types:
  castData(map = pcAuxData)

  ## Check and clean the data:
  if (!simMode)
    cleanData(map = pcAuxData)

  pcAuxData$setTime("reCast")
  if (pcAuxData$checkStatus == "all")
    pcAuxData$setStatus("reCast")

  ## Check for and treat any single nominal variables that are missing
  ## only one datum
  singleMissNom <- with(pcAuxData,
                        (nrow(data) - respCounts == 1) &
                          (typeVec == "binary" |
                             typeVec == "nominal"))
  if (any(singleMissNom))
    pcAuxData$fillNomCell(colnames(pcAuxData$data[singleMissNom]))

  pcAuxData$setTime("nomMis")
  if (pcAuxData$checkStatus == "all")
    pcAuxData$setStatus("nomMis")

  if (verbose > 1)
    cat("\nMultiply imputing missing data...\n")

  ## Construct a predictor matrix for mice():
  if (verbose > 1) {
    cat(
      "--Constructing predictor matrix...",
      "\n MinPcPredCOr =",
      pcAuxData$minPcPredCor,
      "\n MinPcPredCount =",
      pcAuxData$minPcPredCount,
      "\n"
    )
  }
  nLin    <- length(grep("^linPC\\d", colnames(pcAuxData$data)))
  nNonLin <- ifelse(nComps[2] == 0,
                    0,
                    length(grep(
                      "^nonLinPC\\d", colnames(pcAuxData$data)
                    )))

  pcAuxData$predMat <- pcQuickPred(
    data   = pcAuxData$data,
    nLinear      = nLin,
    nNonLinear   = nNonLin,
    mincor       = pcAuxData$minPcPredCor,
    minPredCount = pcAuxData$minPcPredCount
  )
  if (verbose > 1)
    cat("done.\n")

  pcAuxData$setTime("predMat")
  if (pcAuxData$checkStatus == "all")
    pcAuxData$setStatus("predMat")

  ## Specify a vector of elementary imputation methods:
  if (verbose > 1)
    cat("--Creating method vector...")
  pcAuxData$createMethVec(micemethods = micemethods)
  if (verbose > 1)
    cat("done.\n")

  pcAuxData$setTime("methVec")
  if (pcAuxData$checkStatus == "all")
    pcAuxData$setStatus("methVec")

  if (forcePmm & verbose > 1)
    cat("PMM forced by user.\n")

  ## Multiply impute the missing data in 'mergedData':
  if (verbose > 1)
    cat("--Imputing missing values...\n")

  if (nProcess == 1) {
    # Impute in serial
    pcAuxData$miceObject <- try(mice(
      pcAuxData$data,
      m               = nImps,
      maxit           = 1L,
      predictorMatrix = pcAuxData$predMat,
      method          = pcAuxData$methVec,
      printFlag       = verbose == 2,
      ridge           = pcAuxData$miceRidge,
      seed            = pcAuxData$seed,
      nnet.MaxNWts    = pcAuxData$maxNetWts
    ),
    silent = TRUE)

    pcAuxData$setTime("impSerial")
    if (pcAuxData$checkStatus == "all")
      pcAuxData$setStatus("impSerial")

    if (class(pcAuxData$miceObject) != "try-error") {
      pcAuxData$data <- "Removed to save resources."

      ## Complete the incomplete data sets:
      pcAuxData$completeMiData()

      pcAuxData$setTime("completeMi")
      if (pcAuxData$checkStatus == "all")
        pcAuxData$setStatus("completeMi")

    } else {
      errFun("miceCrash", map = pcAuxData)
    }

  } else {
    # Impute in parallel
    myCluster <- makeCluster(nProcess)
    clusterEvalQ(myCluster, library(mice))

    pcAuxData$miDatasets <- parLapply(
      myCluster,
      X       = c(1:nImps),
      fun     = parallelMice,
      map     = pcAuxData,
      tempDirName = tempdir()
    )

    stopCluster(myCluster)

    pcAuxData$setTime("impParallel")
    if (pcAuxData$checkStatus == "all")
      pcAuxData$setStatus("impParallel")

    # assemble list of returned datasets into single miDataset object
    pcAuxData$transformMiData()

    # retrieve mids object for 1st parallel process and put in PcAuxObject
    pcAuxData$miceObject <-
      readRDS(file = file.path(tempdir(), "firstMids.RDS"))

    pcAuxData$setTime("transformMI")
    if (pcAuxData$checkStatus == "all")
      pcAuxData$setStatus("transformMI")

  }

  if (verbose > 1)
    cat("\n--done.\n")
  if (verbose > 1)
    cat("Complete.\n")

  pcAuxData$setTime("miEnd")
  if (pcAuxData$checkStatus == "all")
    pcAuxData$setStatus("miEnd")

  pcAuxData
}# END miWithPcAux()
