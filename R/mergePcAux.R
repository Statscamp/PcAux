#' Merge the Principal Component Auxiliaries with the raw data from which they
#' were created
#'
#' Merge principal components with raw data
#'
#' Merge principal components with raw data
#'
#' @param pcAuxData pcAuxData object
#' @param rawData The raw data to be merged with the principal components
#' @param nComps The number of components to merge
#' @param verbose Toggle verbosity
#' @param ... other function parameters
#' @return A raw data set joined with the principal component auxiliary
#' variables
#' @export mergePcAux
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
