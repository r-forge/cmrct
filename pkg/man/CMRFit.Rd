\name{CMRFit}
\alias{CMRFit}
\alias{print.CMRCTFit}
\title{
Model fitting
}
\description{
'CMRFit' fit a Capture-Mark-Recapture model in continuous time.
}
\usage{
CMRFit(surv, capt, cmrdata, start = 0, method = "BFGS", se.fit = TRUE,
latts = FALSE, ...)
\method{print}{CMRCTFit}(x, \dots)
}

\arguments{
  \item{surv}{an object of class 'formula': a symbolic
    description of the survival model to be fitted.}

  \item{capt}{an object of class 'formula': a symbolic description of
    the capture model to be fitted.}  

  \item{cmrdata}{an object of class CMRData. See
    ?CMRData for details.}

  \item{start}{Initial values for the parameters to be optimized
  over. Either a single value applying for each parameters (0 fr each
  parameter by default) or a numeric
  vector containing a starting value for each parameter.}

  \item{method}{the method to be used in optim (see ?optim for
  details). Default is 'BFGS'.}

  \item{se.fit}{logical. If 'TRUE' (the default) the standad error of
    the parameter is derived from the Hessian matrix.}

  \item{latts}{logical. This argument is only present for experimental purpose and should not be
  used by the user.}

  \item{x}{an object of class 'CMRCTFit'.}
  
  \item{\dots}{additional arguments to be passed to or from other methods.}

}

\details{
  Fit a Capture-Mark-Recapture model in continuous time.
}
\value{
  An object of class 'CMRCTFit' containing the following components:

  \item{Survival}{a data.frame containing the survival parameter
  estimates and their associated standard errors (only if 'se.fit=TRUE')}

  \item{Capture}{a data.frame containing the capture parameter
  estimates and their associated standard errors (only if 'se.fit=TRUE')}
}

\references{ Fouchet D., Santin-Janin H., Sauvage F., Yoccoz N.-G. &
Pontier D. (submitted) An R package for analyzing survival using
continuous-time open capture-recapture models: an example with feral cats on sub-Antarctic islands.}


\author{
Hugues Santin-Janin (hugues_sj@yahoo.fr) and David Fouchet (david.fouchet@univ-lyon1.fr)
}

\seealso{
CMRData
}

\examples{

\dontrun{
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

##Fit the model
fit <- CMRFit(surv=surv, capt=capt, cmrdata=simdata)
fit

}
}


\keyword{Model fitting}


