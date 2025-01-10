#' Helper function inside of boot_optimism1.
#
#' @description Function to be used in function boot_optimism and in boot_optimismDCA.
#
#' @param data Dataset which provides the required columns (see \strong{Details}).
#
#' @details Dataset must contain the outcome and all eight predictor variables. I removed the three dots for further function arguments from the original function, because we do not use them.
#
#' @author Stephen Rhodes
#
#' @importFrom stats binomial family glm
#
#' @examples
#' # This function has not been modified, except for setting the
#' # outcome to 'newAud' (new onset alcohol use disorder)
#
#' @export
#
# Original source of this function: R package pminternal
# Only adaption: Outcome = newAud
# model_fun is required by the adapted function boot_optimism1 and boot_optimismDCA
model_fun <- function(data){
    glm(newAud ~ ., data=data, family="binomial")
}
