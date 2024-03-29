% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepData.R
\name{prepData}
\alias{prepData}
\title{Prepare Data for Extracting Principal Component Auxiliaries}
\usage{
prepData(
  rawData,
  moderators = NULL,
  nomVars = NULL,
  ordVars = NULL,
  idVars = NULL,
  dropVars = NULL,
  groupVars = NULL,
  simMode = FALSE,
  nProcess = 1L,
  verbose = 2L,
  control,
  ...
)
}
\arguments{
\item{rawData}{A data frame from which to extract the auxiliary principal
components.}

\item{moderators}{An optional character vector containing names of any
moderator variables to in- clude in the initial, single imputation model.
The variables supplied here will be interacted with all other observed
variables when specifying the initial single im- putation model’s
systematic component. The exact method by which this mod- eration is
incorporated depends on the interactTypeargument in createPcAux (see the
documentation for createPcAux for more information).}

\item{nomVars}{An optional character vector containing names of any nominal
variables (i.e., unordered factors) that exist in rawData.}

\item{ordVars}{An optional character vector containing names of any ordinal
variables (i.e., ordered factors) that exist in rawData.}

\item{idVars}{An optional character vector containing names of any ID
variables that exist in rawData. Any columns ﬂagged as ID variables should
not be represented in nomVars, ordVars, dropVars, or groupVars}

\item{dropVars}{An optional character vector containing names of any nuisance
variables that should be dropped before extracting the auxiliary principal
component scores.}

\item{groupVars}{An optional character vector containing names of any
grouping variables that can be used to create the strata that deﬁne the
groups used by the fall-back group-mean substitution. If continuous
grouping variables are speciﬁed, they are binned via the cut()function with
breaks = control$nGVarCuts.}

\item{simMode}{A logical switch turning ’Simulation Mode’ on or off. In
Simulation Mode all of the automatic data checks will be suppressed. This
mode is intended for use when prepData is being called as part of a Monte
Carlo simulation study in which the data properties are well-known by the
user. This mode should not be used for ’real-world’ data analysis. Defaults
to simMode = FALSE.}

\item{nProcess}{An integer indicating the number of processors to use when
using parallel pro- cessing for the collinearity checks. A value of
nProcess = 1L results in serial processing. Must be less than or equal to
the available number of logical pro- cessing cores. Defaults to nProcess =
1L.}

\item{verbose}{An integer code in 0, 1, 2 deﬁning the verbosity of output
printed to the screen. verbose = 0prints no output; verbose = 1prints all
output except for the mes- sages returned by mice; verbose = 2 prints all
output, including the messages returned by mice. Warnings are always
printed, regardless of the value assigned to verbose. Defaults to verbose =
2.}

\item{control}{An optional list of control parameters (see ’Details’).}

\item{...}{Not currently used.}
}
\value{
Reference Class object of class PcAuxData
}
\description{
Prepare Data for Extracting Principal Component Auxiliaries
}
