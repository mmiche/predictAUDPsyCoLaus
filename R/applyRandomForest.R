#' Apply random forest.
#
#' @description Apply the default random forest model to the data.
#
#' @param dataTest Test subset (20 percent) of the full sample.
#
#' @param dataTrain Training subset (80 percent) of the full sample.
#
#' @param frmla.f An object of the class \code{formula}, used for the random forest model (f = outcome must be of class \code{factor}).
#
#' @param outcome Character string. Name of the outcome variable, in this study it was 'firstAud' (first onset of alcohol use disorder).
#
#' @return a list with two data.frames as elements (names: ApparentCV, TestCV), each data.frame having three columns:
#' \enumerate{
#' \item observed Observed outcome (0 = absent, 1 = present).
#' \item predicted Model-based probability estimation of the outcome being present.
#' \item ids Row numbers of the total sample (before data has been split into training and test subsets).
#' }
#
#' @author Marcel Miché
#
#' @importFrom ranger ranger
#' @importFrom stats predict
#
#' @examples
#' # See the accompanying R script and this package's vignette,
#' # section Repeated k-fold cross-validation.
#
#' @references
#'
#' \insertRef{ranger2017}{predictAUDPsyCoLaus}
#
#' @export
#
applyRandomForest <- function(dataTrain=NULL, dataTest=NULL, frmla.f=NULL, outcome="y") {
    
    if(is.null(frmla.f)) {
        rfPred <- ranger::ranger(factor(y) ~ ., data = dataTrain, probability = TRUE)
    } else {
        rfPred <- ranger::ranger(frmla.f, data = dataTrain, probability = TRUE)
    }
    apparentCV <- predict(object=rfPred, data=dataTrain, type = "response")$predictions[,"1"]
    rfCV <- predict(object=rfPred, data=dataTest, type = "response")$predictions[,"1"]

    randomForestOut <- list()
    randomForestOut[["ApparentCV"]] <- data.frame(observed=dataTrain[,outcome],
                                            predicted=apparentCV,
                                            ids=rownames(dataTrain))
    randomForestOut[["TestCV"]] <- data.frame(observed=dataTest[,outcome],
                                        predicted=rfCV,
                                        ids=rownames(dataTest))
    return(randomForestOut)
}
