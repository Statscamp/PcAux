# PcAux

## Principle Component Auxiliary Variables

This is the repository for the PcAux package which was formerly called "quark."

PcAux assists in automatically extracting auxiliary features for simple,
principled missing data imputation.

PcAux is beta software. Please report any [issues](https://github.com/Statscamp/PcAux/issues) that you encounter. 
You may also suggest new features.

Thank you for your interest in PcAux! We hope you find our software
useful!

## Installation

The `PcAux` package can be installed from GitHub using one of the following
methods:

[pak](https://pak.r-lib.org/index.html)
```
pak::pkg_install("Statscamp/PcAux")
```

[devtools](https://devtools.r-lib.org/index.html)
```
devtools::install_github("Statscamp/PcAux")
```

[remotes](https://remotes.r-lib.org/)
```
remotes::install_github("Statscamp/PcAux")
```

## Documentation
You can find detailed documentation [here](docs). We are working diligently to 
move the documentation directly into the package.

## Example
A basic missing data treatment using `PcAux` might look like the following:

1. Load the `PcAux` Library
```
library(PcAux)
```

2. Load and prepare your data:
```
data(sample1)

# Examine the sample data
head(sample1)
summary(sample1)

# Create variables to pass as function parameters
# List of nominal variables included in imputation
myNoms   <- c("male","incident")

# List of ordinal variables included in imputation
myOrds   <- c("grade")

# Exclude row id variables from the imputation
myIds    <- c("ID")

# List of other variables to exclude from imputation
myDrops  <- c("Qual")

# List of moderators you plan on using in your analysis model
myMods   <- c("grade","incident")
```

3. Create the PcAux object by passing the raw data and function parameters to
`prepData`
```
pcaux_obj1 <-
  prepData(
    rawData    = sample1,
    moderators = myMods,
    nomVars    = myNoms,
    ordVars    = myOrds,
    idVars     = myIds,
    dropVars   = myDrops
  )
```

4. Create a set of principal component auxiliary variables
```
pcaux_obj2 <- createPcAux(
  pcAuxData = pcaux_obj1,
  nComps = c(Inf, Inf),
  interactType = 2,
  maxPolyPow = 1,
  control = list(minItemPredCor = .3)
)
```

5. Finally, use the principal component auxiliaries as the predictors in a
   multiple imputation run:
```
pcaux_obj3 <-
  miWithPcAux(
    rawData = sample1,
    pcAuxData = pcaux_obj2,
    nComps = c(.6, .5),
    nImps = 10
  )
```

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
