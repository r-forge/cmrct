\name{CMRData}
\alias{CMRData}
\alias{print.CMRCTData}

\title{
Format Capture-Mark-Recapture data	
}

\description{
Create an object of class CMRData suitable for a call to CMRFit.
}

\usage{
CMRData(indvar, sptvar = NULL, captocc, tps = NULL, unit = "year", clage = NULL)
\method{print}{CMRCTData}(x, \dots)
}

\arguments{
  \item{indvar}{a data.frame containing individual covariates. It must at least contains the columns: 'ind' (factor) identifying each individuals, 'date' (POSIXct) the date of each capture of each individuals, 'pop' (factor) identifying the population to which each individual pertain. All other columns are considered as individuals covariates usable to model survival and/or capture probabilities. To model age effect 'indvar' must contain a column 'age' (numeric) containing age at first capture for each individuals. 'indvar' must contain only one line per capture i.e., 'date' of capture of a given individuals must be unique. Missing values are not allowed. }
  
  \item{sptvar}{(optional) a named list of data.frame containing temporal or spatio-temporal variables. Each data.frame must contain at least the columns: 'date' (POSIXct) the date at which the covariables was measured, 'pop' the population where the covariables was measured (for temporal covariates) and/or 'ind' identifying the individual to which the covariables pertain (for spatio-temporal variables). Other column are considered as temporal or spatial-temporal covariate usable to model survival and/or capture probabilities. Each data.frame must contain only one temporal or spatial-temporal covariate. Note that 'T' and 't' are reserved named that should not be present in sptvar. Time covariables 'T' (factor) and 't' (continuous) are by default automatically created by the procedure CMRData.}
	
  \item{captocc}{a data.frame containing the dates of the capture occasions. 'captocc' must contain at least the columns: 'pop' (factor) identifying the population where the capture occasion occured, 'start' (POSIXct) and 'end' (POSIXct) which are respectively the date of beginning and ending of each capture occasions. }

  \item{tps}{(optional) a list with two elements used to construct the variables 'T' and 't'. The elements of 'tps' must be: 'start' (POSIXct) the date at which the variable 'T' and 't' should start, 'step' (character) defining time intervals. 'step' must be defined as the argument 'by' in seq.POSIXt. See ?seq.POSIXt for details. By default 'start' is set to the first january of the year of the first capture occasion and 'step="year"'} 

  \item{unit}{(optional) the temporal resolution of the estimate of survival and capture probabilities. 'unit' also define the 'age' unit. It must be defined as the argument 'by' in seq.POSIXt. See ?seq.POSIXt for details. By default 'unit="year"'.}

  \item{clage}{(optional) only when 'age' is present in 'indvar'. 'clage' must be a numeric vector defining the superior limit of the n-1 first age classes. For instance, to define three age classes 0:1, 1:2 and +2 'clage' should be 'c(1,2)'. See 'unit' to set age unit.}

  \item{x}{an object of class 'CMRCTData'.}
  
  \item{\dots}{additional arguments to be passed to or from other methods.}

}

\details{
The variables names 'T', 't' are reserved and should not be specified by the user in 'indvar' or 'sptvar'. 
}

\value{
An object of class CMRData.
}

\references{ Fouchet D., Santin-Janin H., Sauvage F., Yoccoz N.-G. &
Pontier D. (submitted) An R package for analyzing survival using
continuous-time open capture-recapture models: an example with feral cats on sub-Antarctic islands.}


\author{
Hugues Santin-Janin (hugues_sj@yahoo.fr) and David Fouchet (david.fouchet@univ-lyon1.fr)
}

\seealso{
seq.POSIXt, CMRFit
}

\examples{

\dontrun{
##An example with simulated data
##Let define some arguments
start <- as.POSIXct(strptime("01/01/2000", "\%d/\%m/\%Y"))
factind <- list(sex=c("M", "F"))
##numind <- list(weight=c(0.5, 4))
numtps <- list(rain=c(0, 100))

##Simulate the set of covariables
set.seed(123)
simvars <- CMRSimVars(n=15 , npop=4, start=start, step="6 months",
nstep=12, factind=factind, numtps=numtps, length="5 weeks",
clage=c(2), minage=0.1, maxage=20)

##Plot the simulated capture occasions
plot(simvars)

##True model
surv <- as.formula(~pop + age)
capt <- as.formula(~t)

##True model parameters values
#Survival parameters: Intercept, pop2, pop3, pop4, age1
surv.pars <- c(-2, 1.5, -0.1, -0.5, 0.3)
#Capture parameters: Intercept, t
capt.pars <- c(-1, 0.24)
theta <- c(surv.pars, capt.pars)

##Simulating a dataset
set.seed(456)
simdata <- CMRSimData(simvars=simvars, surv=surv, capt=capt, theta=theta)

##Plot the simulated capture occasions and capture histories
plot(simdata)

}
}


\keyword{Data}

