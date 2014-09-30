\name{CMRFit}
\alias{CMRFit}

\title{
Model fitting
}
\description{
'CMRFit' fit a Capture-Mark-Recapture model in continuous time.
}
\usage{
CMRFit(surv, capt, cmrdata, start = 0, method = "BFGS", se.fit = TRUE, latts = FALSE, ...)
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

  \item{\dots}{additional arguments to be passed to the optim function (see below).}
}

\details{
ffzerfzerf
}
\value{
  An object of class 'CMRFit' containing the following components

  \item{Survival}{a data.frame containing the survival parameter
  estimates and their associated stanard error (only if 'se.fit=TRUE')}

  \item{Capture}{a data.frame containing the capture parameter
  estimates and their associated stanard error (only if 'se.fit=TRUE')}
}

\references{ Fouchet D., Santin-Janin H., Sauvage F., Yoccoz N.-G. &
Pontier D. (submitted) An R package for analysing survival from
capture-mark-recapture non-standard protocols using continuous time
capture-recapture models.}

\author{
H. Santin-Janin (hugues.santin-janin@oncfs.gouv.fr) and D. Fouchet (david.fouchet@univ-lyon1.fr)
}

\seealso{
CMRData
}

\examples{
plot(-1:10, main="CMRFit")
}


\keyword{Model fitting}

