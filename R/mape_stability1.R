#' Prepare MAPE stability plot data.
#
#' @description Extract data for mean average prediction error (MAPE) stability.
#
#' @param x Output of function \code{validate} from the \code{pminternal} package.
#
#' @details This function is a modification of the function \code{mape_stability} of the R package \code{pminternal}. The reason to modify it, was that I wanted to visualize the results with ggplot2, not with the R graphics package.
#
#' @return a list with two elements:
#' \enumerate{
#' \item mapeDf Dataframe with as many rows as the original data and with two columns (p_app = apparent predicted probabilities from the original full data, individual_mape = mean of the absolute prediction error of each individual).
#' \item out List with two elements: A vector with the individual mean absolute prediction errors, the overall average MAPE.
#' }
#' See also Riley and Collins, 2023, section 3.2.4.
#
#' @author Stephen Rhodes (see \strong{Details})
#
#' @importFrom pminternal get_stability
#
#' @examples
#' # See the accompanying R script and this package's vignette,
#' # section MAPE stability (MAPE = mean absolute prediction error).
#
#' @references
#'
#' \insertRef{pminternal2024}{predictAUDPsyCoLaus}
#
#' @export
#
mape_stability1 <- function(x) {
    stabil <- pminternal::get_stability(x)
    stabil <- stabil$stability
    b <- ncol(stabil) - 1
    p_orig <- stabil[, 1]
    p_boot <- stabil[, -1]
    pe <- apply(p_boot, 2, function(pb) abs(p_orig - pb))
    individual_mape <- apply(pe, 1, mean)
    average_mape <- mean(individual_mape)
    out <- list(individual_mape = individual_mape, average_mape = average_mape)
    stabilDf <- data.frame(stabil)
    colnames(stabilDf)[-1] <- paste0("b", seq(b))
    stabilLs <- list()
    stabilLs[["mapeDf"]] <- data.frame(p_app=p_orig, individual_mape=individual_mape)
    stabilLs[["out"]] <- out
    return(stabilLs)
}
