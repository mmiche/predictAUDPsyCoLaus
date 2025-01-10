#' Helper function inside of boot_optimism1.
#
#' @description Function to be used in function boot_optimism and in boot_optimismDCA.
#
#' @param data Dataset which provides the required columns (see \strong{Details}).
#
#' @param model Output of a fitted logistic regression model.
#
#' @details Dataset must contain the outcome and all eight predictor variables. I removed the three dots for further function arguments from the original function, because we do not use them.
#
#' @author Stephen Rhodes
#
#' @importFrom stats predict
#
#' @examples
#' # This function has not been modified. It is used in boot_optimism1
#' # and in boot_optimismDCA.
#
#' @export
#
# Original source of this function: R package pminternal
# pred_fun is required by the adapted function boot_optimism1 and boot_optimismDCA
pred_fun <- function(model, data){
    predict(model, newdata=data, type="response")
}
