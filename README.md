# PcAux

## Principle Component Auxiliary Variables

This is the repository for the PcAux package which was formerly called "quark."

PcAux assists in automatically extracting auxiliary features for simple,
principled missing data imputation.

PcAux is beta software. Please report any [issues](https://github.com/Statscamp/PcAux/issues) that you encounter. 
You may also suggest new features in the issues section.

Thank you for your interest in PcAux! We hope you find our software
useful!

## Installation

The `PcAux` package can be installed from GitHub using any of the following
methods:

[pak](https://pak.r-lib.org/index.html)
```
pak::pkg_install("Statscamp/PcAux")
```

[devtools](https://devtools.r-lib.org/index.html)
```
devtools::install_github("Statscamp/PcAux")
```

## Example
A basic missing data treatment using **PcAux** might look like the following:

1. First, load and prepare your data:

        data(iris2)
        cleanData <- prepData(rawData   = iris2,
                              nomVars   = "Species",
                              ordVars   = "Petal.Width",
                              idVars    = "ID",
                              dropVars  = "Junk",
                              groupVars = "Species")

2. Next, create a set of principal component auxiliary variables:

        pcAuxOut <- createPcAux(pcAuxData = cleanData,
                                nComps    = c(3, 2))

3. Finally, use the principal component auxiliaries as the predictors in a
   multiple imputation run:

        miOut <- miWithPcAux(rawData   = iris2,
                             pcAuxData = pcAuxOut,
                             nImps     = 5)

You can also work directly with the principal component auxiliaries:

- You can merge the principal component auxiliaries back onto your raw data (e.g.,
  for use with the Graham, 2003, saturated correlates approach).

        outData <- mergePcAux(pcAuxData = pcAuxOut, rawData = iris2)

- You can also create a stand-alone predictor matrix that can be used to
  correctly incorporate the principal component auxiliaries into a separate
  MI run using the **mice** package.

        predMat <- makePredMatrix(mergedData = outData)

[builds]:  https://github.com/PcAux-Package/PcAux/tree/master/builds/
[docs]:    https://github.com/PcAux-Package/PcAux/tree/master/documentation/
[src]:     https://github.com/PcAux-Package/PcAux/tree/master/source/PcAux
[LICENSE]: https://github.com/PcAux-Package/PcAux/blob/master/LICENSE
