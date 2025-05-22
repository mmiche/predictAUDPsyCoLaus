#' Prepare prediction stability plot data.
#
#' @description Extract data for prediction stability.
#
#' @param bounds Width of the 'stability interval' (percentiles of the bootstrap model predictions). NULL = do not add bounds to plot.
#
#' @param smooth_bounds Boolean value (default = FALSE). If TRUE, uses loess to smooth the bounds; see Argument \code{smooth_bounds} in \code{pminternal::prediction_stability}.
#
#' @param span Numeric value. Controls the degree of smoothing (see loess; default = 0.75); see Argument \code{span} in \code{pminternal::prediction_stability}.
#
#' @param x Output of function \code{validate} from the \code{pminternal} package.
#
#' @details This function is a modification of the function \code{prediction_stability} of the R package \code{pminternal}. The reason to modify it, was that I wanted to visualize the results with ggplot2, not with the R graphics package.
#
#' @return a list with three lists as elements:
#' \enumerate{
#' \item b Number of bootstrap repetitions (we used b = 500).
#' \item stabilDf Dataframe with as many rows as the original data and with b + 1 columns (column 1 = p_app = apparent predicted probability, whereas the remaining b many columns contain the cross-validated predicted probabilities).
#' \item lims Dataframe with as many rows as the original data and with three columns (names: X2.5., X.97.5, and p_app). In the prediction instability plot, they are shown as the dashed 95 percent stability interval for the estimated risks from the bootstrap models (see Riley and Collins, 2023, section 3.2.1).
#' }
#
#' @author Stephen Rhodes (see \strong{Details})
#
#' @importFrom pminternal get_stability
#' @importFrom stats loess predict quantile
#
#' @examples
#' # See the accompanying R script and this package's vignette,
#' # section Prediction stability.
#
#' @references
#'
#' \insertRef{pminternal2025}{predictAUDPsyCoLaus}
#
#' @export
#
prediction_stability1 <- function(x, bounds = 0.95, smooth_bounds = FALSE, span = .75) {
    stabil <- pminternal::get_stability(x)
    stabil <- stabil$stability
    b <- ncol(stabil) - 1
    stabilLs <- list(b=b)
    xlab <- "Estimated risk from development model"
    ylab <- sprintf("Estimated risk from bootstrap models (n = %i)", b)
    stabil = stabil[order(stabil[, 1]), ]
    probs <- c((1 - bounds)/2, 1 - (1 - bounds)/2)
    lims <- t(apply(stabil[, -1], 1, quantile, probs = probs))
    if(smooth_bounds) {
        lims <- apply(lims, 2, function(x) predict(loess(x ~ stabil[, 1], span = span)))
    }
    colnames(stabil)[-1] <- paste0("b", seq(b))
    rownames(stabil) <- NULL
    stabilLs[["stabilDf"]] <- data.frame(stabil)
    limsDf <- data.frame(lims)
    limsDf[,"p_app"] <- stabil[,1]
    rownames(limsDf) <- NULL
    stabilLs[["lims"]] <- data.frame(limsDf)
    return(stabilLs)
}
