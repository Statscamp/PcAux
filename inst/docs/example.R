pak::pkg_install("Statscamp/PcAux@bug-fix")
library(PcAux)
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

pcaux_obj1 <-
  prepData(
    rawData    = sample1,
    moderators = myMods,
    nomVars    = myNoms,
    ordVars    = myOrds,
    idVars     = myIds,
    dropVars   = myDrops
  )

pcaux_obj2 <- createPcAux(
  pcAuxData = pcaux_obj1,
  nComps = c(Inf, Inf),
  interactType = 2,
  maxPolyPow = 1,
  control = list(minItemPredCor = .3)
)

pcaux_obj3 <-
  miWithPcAux(
    rawData = sample1,
    pcAuxData = pcaux_obj2,
    nComps = c(.6, .5),
    nImps = 10
  )

library(purrr)
detach(purrr)
