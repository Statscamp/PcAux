# PcAux
This is the repository for the PcAux package which was formerly called "quark."

- Licensing information is given in the [LICENSE](LICENSE.md) file.
- Built tarballs of the PcAux package are available in the [builds][] directory.
- Stand-alone documentation is available in the [documentation][docs] directory.
- The source files for the most recent stable version of PcAux are available in
  the [source][src] directory.

PcAux is beta software, so please report any bugs that you encounter in the
issues section of the project page. You may also leave requests for new features
in the issues section.

Thank you for your interest in the PcAux project! I hope you find our software
useful!

# Installation
## pak
```
pak::pkg_install("Statscamp/PcAux")
```

## devtools
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
