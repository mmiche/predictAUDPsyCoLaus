% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snbRecalPlotNum.R
\name{snbRecalPlotNum}
\alias{snbRecalPlotNum}
\title{Recalibration plot data.}
\usage{
snbRecalPlotNum(
  p,
  p.std,
  p.const,
  y,
  r,
  stdErrThresh = 1,
  risk.model.std = TRUE
)
}
\arguments{
\item{p}{Vector of risk score values (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).}

\item{p.std}{Vector of risk score values after standard logistic recalibration (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).}

\item{p.const}{Vector of risk score values after constrained logistic recalibration (argument added by us).}

\item{y}{Vector of integer values that are either 0 (= outcome not observed) or 1 (= outcome observed).}

\item{r}{Numeric value. Clinically relevant risk threshold (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).}

\item{stdErrThresh}{Numeric value (default = 1). Indicates how many standard errors line drawn below the maximum of the sNB curve should be (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).}

\item{risk.model.std}{Boolean value (default = TRUE). Plot standard error bars for sNB of plotted risk models (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).}
}
\value{
a named vector with five elements:
\enumerate{
\item snbMax The maximum possible standardized net benefit (snb), given the specified threshold probability.
\item snbOrig The snb which is based on the logistic regression model (no recalibration).
\item snbStdRecal The snb which is based on the standard recalibration method by Cox (1954).
\item snbConstRecal The snb which is based on the contrained logistic recalibration method by Mishra et al. (2022).
\item oneSEfromMax One standard error away from snbMax.
}
Note: The one standard error distance from snbMax is shown as dashed horizontal line in Figures 1, 4, and 5 in Mishra et al. (2022).
}
\description{
Extract standardized net benefit results of the recalibration plot.
}
\details{
Marcel Miché slightly modified the function snbRecalPlot from the package \code{ClinicalUtilityRecal}, in order to be able to collect the results in numeric form.
}
\examples{
# See the accompanying R script and this package's vignette,
# section Recalibration: Potential for improved net benefit?
}
\references{
\insertRef{ClinicalUtilityRecal2020}{predictAUDPsyCoLaus}
}
\author{
Anu Mishra (see \strong{Details})
}
