\name{CMRSimData}
\alias{CMRSimData}

\title{
Simulate an object of class 'CMRCTData'.
}

\description{
Simulate an object of class 'CMRCTData' suitable for a call to 'CMRFit'.
}

\usage{ CMRSimData(simvars, surv, capt, theta) }

\arguments{

 \item{simvars}{an object of class 'CMRCTSimVars'. See '?CMRSimVars' for
 details.}

 \item{surv}{an object of class 'formula': a symbolic
   description of the true survival model to be used for simulating the data.}
 
 \item{capt}{an object of class 'formula': a symbolic description of
   the true capture model to be used for simulating the data.}  
 
 \item{theta}{a vector (numeric) containing respectively the survival
 and capture parameters values.}
 
}

\details{ The variables names 'T' (time defined as factor), 't' (time
defined as numeric) are reserved and should not be present in
'simvars'. They will be automatically generated when calling
'CMRSimData'.}

\value{
An object of class 'CMRCTData'.
}

\references{ Fouchet D., Santin-Janin H., Sauvage F., Yoccoz N.-G. &
Pontier D. (submitted) An R package for analysing survival from
capture-mark-recapture non-standard protocols using continuous time
capture-recapture models.}

\author{
H. Santin-Janin (hugues.santin-janin@oncfs.gouv.fr) and D. Fouchet (david.fouchet@univ-lyon1.fr)
}

\seealso{
CMRSimVars, CMRFit
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
surv.pars <- c(-2, 1.5, -0.1, -0.5, 0.3)
capt.pars <- c(-1, 0.24)
theta <- c(surv.pars, capt.pars)

##Simulating a dataset
set.seed(456)
simdata <- CMRSimData(simvars=simvars, surv=surv, capt=capt, theta=theta)

##Plot the simulated capture occasions and capture histories
plot(simdata)

}
}


\keyword{Data Simulation}

