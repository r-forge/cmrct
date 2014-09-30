\name{CMRData}
\alias{CMRData}

\title{
Format Capture-Mark-Recapture data	
}

\description{
Create an object of class CMRData suitable for a call to CMRFit.
}

\usage{
CMRData(indvar, sptvar = NULL, captocc, tps = NULL, unit = "year", clage = NULL)
}

\arguments{
  \item{indvar}{a data.frame containing individual covariates. It must at least contains the columns: 'ind' (factor) identifying each individuals, 'date' (POSIXct) the date of each capture of each individuals, 'pop' (factor) identifying the population to which each individual pertain. All other columns are considered as individuals covariates usable to model survival and/or capture probabilities. To model age effect 'indvar' must contain a column 'age' (numeric) containing age at first capture for each individuals. 'indvar' must contain only one line per capture i.e., 'date' of capture of a given individuals must be unique. Missing values are not allowed. }
  
  \item{sptvar}{(optional) a named list of data.frame containing temporal or spatio-temporal variables. Each data.frame must contain at least the columns: 'date' (POSIXct) the date at which the covariables was measured, 'pop' the population where the covariables was measured (for temporal covariates) and/or 'ind' identifying the individual to which the covariables pertain (for spatio-temporal variables). Other column are considered as temporal or spatial-temporal covariate usable to model survival and/or capture probabilities. Each data.frame must contain only one temporal or spatial-temporal covariate. Note that 'T' and 't' are reserved named that should not be present in sptvar. Time covariables 'T' (factor) and 't' (continuous) are by default automatically created by the procedure CMRData.}
	
  \item{captocc}{a data.frame containing the dates of the capture occasions. 'captocc' must contain at least the columns: 'pop' (factor) identifying the population where the capture occasion occured, 'start' (POSIXct) and 'end' (POSIXct) which are respectively the date of beginning and ending of each capture occasions. }

  \item{tps}{(optional) a list with two elements used to construct the variables 'T' and 't'. The elements of 'tps' must be: 'start' (POSIXct) the date at which the variable 'T' and 't' should start, 'step' (character) defining time intervals. 'step' must be defined as the argument 'by' in seq.POSIXt. See ?seq.POSIXt for details. By default 'start' is set to the first january of the year of the first capture occasion and 'step="year"'} 

  \item{unit}{(optional) the temporal resolution of the estimate of survival and capture probabilities. 'unit' also define the 'age' unit. It must be defined as the argument 'by' in seq.POSIXt. See ?seq.POSIXt for details. By default 'unit="year"'.}

  \item{clage}{(optional) only when 'age' is present in 'indvar'. 'clage' must be a numeric vector defining the superior limit of the n-1 first age classes. For instance, to define three age classes 0:1, 1:2 and +2 'clage' should be 'c(1,2)'. See 'unit' to set age unit.}
}

\details{
The variables names 'T', 't' are reserved and should not be specified by the user in 'indvar' or 'sptvar'. 
}

\value{
An object of class CMRData.
}

\references{ Fouchet D., Santin-Janin H., Sauvage F., Yoccoz N.-G. &
Pontier D. (submitted) An R package for analysing survival from
capture-mark-recapture non-standard protocols using continuous time
capture-recapture models.}

\author{
H. Santin-Janin (hugues.santin-janin@oncfs.gouv.fr) and D. Fouchet (david.fouchet@univ-lyon1.fr)
}

\seealso{
seq.POSIXt, CMRFit
}

\examples{
plot(1:10, main="CMRData")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Data}

