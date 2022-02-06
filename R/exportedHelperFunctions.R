#' Create the PcAuxData object, calculate interactions, do PCA
#'
#' Second step of the pcaux imputation process
#'
#' Second step of the pcaux imputation process
#'
#' @export pcAuxW

## Print the (lack of) warranty information:
pcAuxW <- function() {
  lic <- readLines(system.file("LICENSE", package = "PcAux"))

  start <- grep("15. Disclaimer of Warranty", lic)
  end   <- grep("END OF TERMS AND CONDITIONS", lic) - 1

  writeLines(lic[start:end])
}


## Print license:
pcAuxL <- function()
  writeLines(readLines(system.file("LICENSE", package = "PcAux")))


### Merge the Principal Component Auxiliaries with the raw data from which they
### were created:
mergePcAux <-
  function(pcAuxData,
           rawData,
           nComps = NULL,
           verbose = TRUE,
           ...)
  {
    args   <- list(...)
    idVars <- pcAuxData$idVars
    varExp <- c(NA, NA)

    if (missCheck(nComps)) {
      ## If no explicit number of components is defined, use all available:
      nComps <- pcAuxData$nComps
    } else {
      ## Ascertain the number of components to use:
      for (i in c(1, 2)) {
        r2 <- pcAuxData$rSquared[[i]]
        nc <- nComps[i]
        if (is.infinite(nc)) {
          tmp       <-  which(r2[-length(r2)] == r2[-1])
          nComps[i] <- ifelse(length(tmp) == 0, length(r2), tmp[1])
        } else if (nc < 1 & nc > 0) {
          varExp[i] <- nComps[i]
          nComps[i] <- sum(r2 < nc) + 1
        } else if (nc < 0) {
          nComps[i] <- length(r2)
        }
      }

      ## Must use at least 1 linear PcAux
      check <- nComps[1] == 0
      if (check)
        errFun("noLinPcAux", creatingPcAux = FALSE)

      ## Make sure non-linear PcAux are available, if requested
      check <- pcAuxData$nComps[2] == 0 & nComps[2] > 0
      if (check)
        errFun("missingNonLinPcAux")

      ## Requesting a legal number of linear components?
      check <- nComps[1] <= pcAuxData$nComps[1]
      if (check) {
        if (verbose) {
          # How much variance is explained?
          print(
            paste0(
              "NOTE: ",
              nComps[1],
              " component ",
              ifelse(nComps[1] > 1,
                     "scores explain ",
                     "score explains "),
              round(100 * pcAuxData$rSquared$lin[nComps[1]]),
              "% of the variance in 'rawData.'"
            )
          )
        }
      } else {
        if (is.na(varExp[1]))
          # Argument -> Component counts
          errFun("fewLinPcAux", pcAuxData = pcAuxData, nComps = nComps)
        else
          # Argument -> Variance explained
          errFun("linVarExp", pcAuxData = pcAuxData, varExp = varExp)
      }

      ## Requesting a legal number of non-linear components?
      check <- nComps[2] <= pcAuxData$nComps[2]
      if (check) {
        if (verbose & nComps[2] != 0) {
          # How much variance is explained?
          print(
            paste0(
              "NOTE: ",
              nComps[2],
              " component ",
              ifelse(nComps[2] > 1,
                     "scores explain ",
                     "score explains "),
              round(100 * pcAuxData$rSquared$nonLin[nComps[2]]),
              "% of the variance in the nonlinear ",
              "expansion of 'rawData.'"
            )
          )
        }
      } else {
        if (is.na(varExp[2]))
          # Argument -> Component counts
          errFun("fewNonLinPcAux",
                 pcAuxData = pcAuxData,
                 nComps = nComps)
        else
          # Argument -> Variance explained
          errFun("nonLinVarExp",
                 pcAuxData = pcAuxData,
                 varExp = varExp)
      }
    }# CLOSE if(missCheck(nComps))

    badId <- FALSE

    ## Check for shared ID variables:
    check <- idVars %in% colnames(rawData) &
      idVars %in% colnames(pcAuxData$pcAux$lin)

    if (nComps[2] > 0)
      check <- check & idVars %in% colnames(pcAuxData$pcAux$nonLin)

    if (!any(check)) {
      warnFun("mergeNoID")
      badId <- TRUE
    } else {
      idVars <- idVars[check]

      ## Check that ID variables are unique row-identifiers:
      check <-
        unlist(lapply(as.data.frame(pcAuxData$pcAux$lin[, idVars]),
                      function(x)
                        length(unique(x)) == length(x)))
      if (any(check)) {
        # At least one viable ID variable

        ## Arbitrarily select an acceptable ID variable to use for matching:
        useId    <- idVars[check][1]
        dataId   <- rawData[, useId]
        extraIds <- setdiff(idVars, useId)

        ## Temporarily cast factor-valued raw data IDs as character:
        check <- "factor" %in% class(dataId)
        if (check)
          dataId <- as.character(dataId)

        idMissPat <- is.na(dataId)

        ## Fill missing ID values with dummy levels from the PcAux object:
        if (any(idMissPat))
          ## KML 2016-JUL-31: Check class of 'idFills' to avoid crashes
          ## with one incomplete ID
          if (is.list(pcAuxData$idFills))
            dataId[idMissPat] <- pcAuxData$idFills[[useId]]
        else
          dataId[idMissPat] <- pcAuxData$idFills
      } else {
        # No viable ID variables
        warnFun("mergeBadID", pcAuxData)
        badId <- TRUE
      }
    }

    ## Are we using any non-linear PcAux?
    useNonLin <- pcAuxData$intMeth != 1 & nComps[2] > 0

    ## Merge the PcAux scores onto the raw data:
    linPcNames    <- paste0("linPC",    c(1:nComps[1]))
    if (useNonLin)
      nonLinPcNames <- paste0("nonLinPC", c(1:nComps[2]))

    if (badId) {
      if (useNonLin)
        outData <-
          data.frame(rawData,
                     pcAuxData$pcAux$lin[, linPcNames],
                     pcAuxData$pcAux$nonLin[, nonLinPcNames])
      else
        outData <-
          data.frame(rawData, pcAuxData$pcAux$lin[, linPcNames])
    } else {
      linPcNames    <- c(useId, linPcNames)
      if (useNonLin)
        nonLinPcNames <- c(useId, nonLinPcNames)

      if (useNonLin)
        tmp <- merge(pcAuxData$pcAux$lin[, linPcNames],
                     pcAuxData$pcAux$nonLin[, nonLinPcNames])
      else
        tmp <- pcAuxData$pcAux$lin[, linPcNames]
      outData <- merge(rawData, tmp, by = useId)
    }
    if (!is.null(args$intern) &&
        args$intern)
      pcAuxData$data <- outData
    else
      outData
  }# END mergePcAux()


## New Predictor Matrix Method to ensure no PCs below a certain threshhold are used
## Also protects against nPredictors > nObservations iteratively
pcQuickPred <- function(data,
                        mincor = .1,
                        minPredCount = 1,
                        nLinear = NULL,
                        nNonLinear = NULL) {
  # library(coop)

  if (nNonLinear > 0)
    PCs    <- c(paste0("linPC", c(1:nLinear)),
                paste0("nonLinPC", c(1:nNonLinear)))
  else
    PCs    <- paste0("linPC", c(1:nLinear))


  itemNames <- setdiff(names(data), PCs)

  nvar <- ncol(data)
  pcPredictorMatrix <- matrix(
    0,
    nrow = nvar,
    ncol = nvar,
    dimnames = list(names(data), names(data))
  )

  # get data and missing pattern as matricies                                                                                                                                                names(data)))
  dataMat <- data.matrix(data)
  rMat <- !is.na(dataMat)

  # get y-corrs
  suppressWarnings(yCor <-
                     abs(coop::pcor(dataMat, use = "pairwise.complete.obs")))
  yCor[is.na(yCor)] <- 0

  # get r-corrs - unlike cor() pcor() cannot accept logical matrix. To force
  # numeric while preserving matrix dimensions, we use rMat*rMat
  suppressWarnings(rCor <-
                     abs(coop::pcor(rMat * rMat, use = "pairwise.complete.obs")))
  rCor[is.na(rCor)] <- 0

  # get larger of yCor and rCor, but only consider r-corr when corresponding
  # y-corr > mincor
  corMat <- pmax(yCor, pmin(rCor, yCor > mincor))
  rownames(corMat) <- names(data)
  colnames(corMat) <- names(data)

  # select all predictors > min threshold
  pcPredictorMatrix[corMat > mincor] <- 1

  # only allow PCs to predict - remove items
  pcPredictorMatrix[, itemNames] <- 0

  # no self-prediction
  diag(pcPredictorMatrix) <- 0

  # no predictors all PCs (because complete) nor any complete items
  pcPredictorMatrix[colSums(!rMat) == 0,] <- 0

  # get max predictors given observed cases -- handle odd case of all cors==0
  maxPredCounts <- pmax(colSums(rMat[, itemNames]) - 1, 0)

  names(maxPredCounts) <- itemNames

  # get list of decreasing PC-item correlations for each item
  rankedCorsList <- purrr::map(itemNames, function(c) {
    itemCors <- corMat[c, PCs]
    names(itemCors) <- PCs
    itemCors <- itemCors[order(itemCors, decreasing = TRUE)]
  })
  names(rankedCorsList) <- itemNames

  for (iName in itemNames) {
    if (sum(pcPredictorMatrix[iName, ]) > maxPredCounts[iName]) {
      keep <- rankedCorsList[[iName]][1:maxPredCounts[iName]]
      keepFlags <- colnames(pcPredictorMatrix) %in% names(keep)
      pcPredictorMatrix[iName,!keepFlags] <- 0
    } # end if

    # enforce minPredCount
    force <- rankedCorsList[[iName]][1:minPredCount]
    forceFlags <- colnames(pcPredictorMatrix) %in% names(force)
    pcPredictorMatrix[iName, forceFlags] <- 1

  } # next iName

  return(pcPredictorMatrix)
} # end pcQuickPred

## Wrapper function to give S3/S4-like access to fields:
inspect <- function(object, what)
  object$field(what)


## Wrapper function to return the imputed data sets:
getImpData <- function(pcAuxData)
  pcAuxData$miDatasets


##Compute elapsed time at each interval:
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


## Write machine status to a text files
writeStatus <- function(pcAuxData, outName, what) {
  utils::capture.output(pcAuxData$status[[what]], file = outName)
  paste("Wrote status to", outName)
}


getLoggedEvents <- function(pcAuxData) {
  if (!nrow(pcAuxData$loggedEvents))
    return("No logged events")
  else
    return(pcAuxData$loggedEvents)
}
