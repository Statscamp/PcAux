#' New Predictor Matrix Method to ensure no PCs below a certain threshhold are
#' used. Also protects against nPredictors > nObservations iteratively.
#'
#' @param data pcAuxData object
#' @param mincor Minimum correlation
#' @param minPredCount Minimum number of predictors
#' @param nLinear Number of linear
#' @param nNonLinear Number of non-linear
#' @return mice predictor matrix
#' @export pcQuickPred

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
}
