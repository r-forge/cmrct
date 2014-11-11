\name{CMRSimVars}
\alias{CMRSimVars}
\alias{print.CMRCTVars}

\title{
Simulate a set af co-Variables
}

\description{
Simulate a set of co-Variables in a format suitable for a call to CMRSimData
}

\usage{CMRSimVars(n=10, npop=4, start, step="6 months", nstep=24, length="5 weeks", factind=NULL,
           	numind=NULL, numtps=NULL, numspts=NULL,	step.spts="month",clage=NULL, 
                minage=NULL, maxage=NULL)
\method{print}{CMRCTVars}(x, \dots)
}

\arguments{

 \item{n}{an integer used to define the total number of individuals to be simulated. Let N=n*p to be the total number of individuals to be simulated with p equal to the product of 'npop' with the product of the number of modalities of all factors in 'factind' and the number of age classes when 'clage' is provoded (i.e., p=npop * prod(sapply(factind, length)) * (length(clage +1))). By default 'n=10'.}

 \item{npop}{the number of populations to simulate.}

 \item{start}{(POSIXct) define beginning of the study period i.e., the date at which the variables 'T' and 't' should start. See ?CMRData for more details.}

 \item{step}{define the age unit and the time step used to define the variables 'T' and 't'. It must be defined as the argument 'by' in seq.POSIXt. See ?seq.POSIXt for details. By default 'step="6 months"'.}

 \item{nstep}{(POSIXct) the number of time step to be simulated. By default 'nstep=10'}

 \item{length}{(POSIXct) the length of the capture occasions. It must be defined as the argument 'by' in seq.POSIXt. By default 'length="5 weeks"'.}

 \item{factind}{(optional) a named list where each element is a character vector containing the levels of a given individual factor to be simulated.}

 \item{numind}{(optional) a named list  where each element is a numeric vector containing respectively the min and max of a given individual covariate (numeric) to be simulated.}

 \item{numtps}{(optional) a named list  where each element is a numeric vector containing respectively the min and max of a given temporal covariate (numeric) to be simulated.}
 
 \item{numspts}{(optional)a named list  where each element is a numeric vector containing respectively the min and max of a given spatio-temporal covariate (numeric) to be simulated.}

 \item{step.spts}{the time step at which each spatio-temporal covariate is updated.}

 \item{clage}{(optional) a numeric vector defining the superior limit of the n-1 first age classes. For instance, to define three age classes 0:1, 1:2 and +2 'clage' should be 'c(1,2)'.}

 \item{minage}{(optional) the minimal age at capture.}

 \item{maxage}{(optional) the maximal age at capture.}
 
 \item{x}{an object of class 'CMRCTVars'.}
 
 \item{\dots}{additional arguments to be passed to or from other methods.}
 
}

\details{ The variables names 'T' (time defined as factor), 't' (time
defined as numeric) are reserved and should not be specified by the user
in 'factind', 'numind' or 'numtps'. They will be automatically generated
when calling 'CMRSimData'. }

\value{
An object of class CMRCTSimVars.
}

\references{ Fouchet D., Santin-Janin H., Sauvage F., Yoccoz N.-G. &
Pontier D. (submitted) An R package for analysing survival from
capture-mark-recapture non-standard protocols using continuous time
capture-recapture models.}

\author{
H. Santin-Janin (hugues.santin-janin@oncfs.gouv.fr) and D. Fouchet (david.fouchet@univ-lyon1.fr)
}

\seealso{
seq.POSIXt, CMRData, CMRFit
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

}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Data Simulation}

