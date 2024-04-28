### Title:        Exported PcAux Helper Functions
### Author:       Kyle M. Lang
### Contributors: Pavel Panko, Vibhuti Gupta, Daniel Bontempo
### Created:      2015-OCT-29
### Modified:     2018-AUG-17

##--------------------- COPYRIGHT & LICENSING INFORMATION --------------------##
##  Copyright (C) 2018 Kyle M. Lang <k.m.lang@uvt.nl>                         ##
##                                                                            ##
##  This file is part of PcAux.                                               ##
##                                                                            ##
##  This program is free software: you can redistribute it and/or modify it   ##
##  under the terms of the GNU General Public License as published by the     ##
##  Free Software Foundation, either version 3 of the License, or (at you     ##
##  option) any later version.                                                ##
##                                                                            ##
##  This program is distributed in the hope that it will be useful, but       ##
##  WITHOUT ANY WARRANTY; without even the implied warranty of                ##
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General  ##
##  Public License for more details.                                          ##
##                                                                            ##
##  You should have received a copy of the GNU General Public License along   ##
##  with this program. If not, see <http://www.gnu.org/licenses/>.            ##
##----------------------------------------------------------------------------##

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

## New Predictor Matrix Method to ensure no PCs below a certain threshhold are used
## Also protects against nPredictors > nObservations iteratively
pcQuickPred <- function(data,
                        mincor = .1,
                        minPredCount = 1,
                        nLinear = NULL,
                        nNonLinear = NULL) {
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

  # get r-corrs - unlike cor pcor cannot accept logical matrix. To force
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
  rankedCorsList <- map(itemNames, function(c) {
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
  capture.output(pcAuxData$status[[what]], file = outName)
  paste("Wrote status to", outName)
}


getLoggedEvents <- function(pcAuxData) {
  if (!nrow(pcAuxData$loggedEvents))
    return("No logged events")
  else
    return(pcAuxData$loggedEvents)
}
