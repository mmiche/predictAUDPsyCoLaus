#' Prepare classification stability plot data.
#
#' @description Extract data for classification stability.
#
#' @param threshold Numeric value between 0 and 1. Estimated risks above the threshold get a predicted 'class' of 1, otherwise 0; see Argument \code{threshold} in \code{pminternal::classification_stability}.
#
#' @param x Output of function \code{validate} from the \code{pminternal} package.
#
#' @details This function is a modification of the function \code{classification_stability} of the R package \code{pminternal}. The reason to modify it, was that I wanted to visualize the results with ggplot2, not with the R graphics package.
#
#' @return a data.frame with as many rows as the original data and two columns:
#' \enumerate{
#' \item p_orig Apparent predicted probability of each individual, based on the original data.
#' \item cii Classificiation instability index, i.e., given a specified probability threshold, how often (in percent) has each individual received the opposite classification, compared to \code{p_orig}?
#' }
#' A perfect prediction model consistently classifies each individual into one class, whereas switching between classes (for each individual across all bootstrap repetitions) indicates classification instability. Note that the classification, which is based on the original data, is used as the ground truth. That is, the apparent predicted probabilities will always produce misclassifications as well, because no prediction model will ever be deterministically perfect.
#' See also Riley and Collins, 2023, section 5.4.
#
#' @author Stephen Rhodes (see \strong{Details})
#
#' @importFrom pminternal get_stability
#
#' @examples
#' # See the accompanying R script and this package's vignette,
#' # section Classification stability.
#
#' @references
#'
#' \insertRef{pminternal2024}{predictAUDPsyCoLaus}
#
#' @export
#
classification_stability1 <- function(x, threshold) {
    stabil <- pminternal::get_stability(x)
    stabil <- stabil$stability
    p_orig <- stabil[, 1]
    c_orig <- p_orig > threshold
    c_boot <- stabil[, -1] > threshold
    ci <- apply(c_boot, 2, function(pcla) c_orig == pcla)
    cii <- 1 - apply(ci, 1, mean)
    return(data.frame(p_orig=p_orig, cii = cii))
}
