#' Prepare decision curve stability plot data.
#
#' @description Extract data for decision curve stability.
#
#' @param thresholds Vector with numeric values. Selected range of reasonable threshold probabilities, we used: .01, .02, .03, .04, and .05.
#
#' @param x Output of function \code{validate} from the \code{pminternal} package.
#
#' @details This function is a modification of the function \code{dcurve_stability} of the R package \code{pminternal}. The reason to modify it, was that I wanted to visualize the results with ggplot2, not with the R graphics package.
#
#' @return a list with b + 1 elements (b = number of bootstrap repetitions). Each element is a dataframe with as many rows as selected threshold probabilities and three columns:
#' \enumerate{
#' \item label Character string, always containing 'Prediction model'.
#' \item threshold Numeric variable with the selected threshold probabilities.
#' \item net_benefit Numeric variable with the net benefit results for each threshold probability.
#' }
#' The first of the list elements contains the net benefits of the apparent performance of the prediction model. The remaining b (we set b = 500) list elements stem from the bootstrap repetitions.
#' See also Riley and Collins, 2023, section 5.3.
#
#' @author Stephen Rhodes (see \strong{Details})
#
#' @importFrom pminternal get_stability
#
#' @examples
#' # See the accompanying R script and this package's vignette,
#' # section Decision curve stability.
#
#' @references
#'
#' \insertRef{pminternal2025}{predictAUDPsyCoLaus}
#
#' @export
#
dcurve_stability1 <- function(x, thresholds) {
    stabil <- pminternal::get_stability(x)
    y <- stabil$y
    stabil <- stabil$stability
    curves <- apply(stabil, 2, function(p) {
        dca_fun(y=y, p=p, thresholds = thresholds)
    })
    return(curves)
}
