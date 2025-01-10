#' Repeatedly apply snbRecalPlotNum.
#
#' @description Function that enables repeated application of snbRecalPlotNum.
#
#' @param p Vector of risk score values (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).
#
#' @param r Numeric value. Clinically relevant risk threshold (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).
#
#' @param y Vector of integer values that are either 0 (= outcome not observed) or 1 (= outcome observed).
#
#' @return Output which is returned by the function \code{snbRecalPlotNum}. In other words, \code{snbVals} is a wrapper function.
#' Note: In a proper R package, a wrapper function remains invisible (a so-called helper function). However, this R package is part of the supplementary material of a publication, which is why we made each function visible.
#
#' @author Marcel Miché
#
#' @importFrom ClinicalUtilityRecal stdRecal constRecal
#
#' @examples
#' # See the accompanying R script and this package's vignette,
#' # section Recalibration: Potential for improved net benefit?
#
#' @references
#'
#' \insertRef{ClinicalUtilityRecal2020}{predictAUDPsyCoLaus}
#
#' @export
#
snbVals <- function(y=NULL, p=NULL, r=NULL) {
    
    recalProbs <- ClinicalUtilityRecal::stdRecal(y=y, p=p)
    
    constrainedRecal <- ClinicalUtilityRecal::constRecal(y=y, p=p, r=r)
    
    return(
        snbRecalPlotNum(
            p=p,
            p.std = recalProbs$p.std,
            p.const = constrainedRecal$p.const,
            y=y,
            r=r,
            risk.model.std = FALSE)
    )
}
