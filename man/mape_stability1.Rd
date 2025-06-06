% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mape_stability1.R
\name{mape_stability1}
\alias{mape_stability1}
\title{Prepare MAPE stability plot data.}
\usage{
mape_stability1(x)
}
\arguments{
\item{x}{Output of function \code{validate} from the \code{pminternal} package.}
}
\value{
a list with two elements:
\enumerate{
\item mapeDf Dataframe with as many rows as the original data and with two columns (p_app = apparent predicted probabilities from the original full data, individual_mape = mean of the absolute prediction error of each individual).
\item out List with two elements: A vector with the individual mean absolute prediction errors, the overall average MAPE.
}
See also Riley and Collins, 2023, section 3.2.4.
}
\description{
Extract data for mean average prediction error (MAPE) stability.
}
\details{
This function is a modification of the function \code{mape_stability} of the R package \code{pminternal}. The reason to modify it, was that I wanted to visualize the results with ggplot2, not with the R graphics package.
}
\examples{
# See the accompanying R script and this package's vignette,
# section MAPE stability (MAPE = mean absolute prediction error).
}
\references{
\insertRef{pminternal2025}{predictAUDPsyCoLaus}
}
\author{
Stephen Rhodes (see \strong{Details})
}
