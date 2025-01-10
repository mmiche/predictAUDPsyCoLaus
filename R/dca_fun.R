#' Helper function inside of boot_optimismDCA.
#
#' @description Function to be used in function boot_optimismDCA.
#
#' @param p Vector of risk score values (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).
#
#' @param thresholds Vector with numeric values. Selected range of reasonable threshold probabilities, we used: .01, .02, .03, .04, and .05.
#
#' @param y Vector of integer values that are either 0 (= outcome not observed) or 1 (= outcome observed).
#
#' @author Marcel Miché
#
#' @examples
#' # This function is used within the function boot_optimismDCA.
#
#' @references
#'
#' \insertRef{mysml2024}{predictAUDPsyCoLaus}
#'
#' \insertRef{vickers2006decision}{predictAUDPsyCoLaus}
#
#' @export
#
dca_fun <- function(y=NULL, p=NULL, thresholds=NULL) {
    dcaTbl <- dca(inputDataset = data.frame(y=y, p=p),
                          truth = "y", prob = "p",
                          selectedThresholds = thresholds)$plotTbl
    dcaTbl$label <- as.character(dcaTbl$label)
    dcaTbl <- dcaTbl[dcaTbl$label=="Prediction model",]
    return(dcaTbl)
}
