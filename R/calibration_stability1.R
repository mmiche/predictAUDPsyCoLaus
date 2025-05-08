#' Prepare calibration stability plot data.
#
#' @description Extract data for calibration stability.
#
#' @param x Output of function \code{validate} from the \code{pminternal} package.
#
#' @details This function is a modification of the function \code{calibration_stability} of the R package \code{pminternal}. The reason to modify it, was that I wanted to visualize the results with ggplot2, not with the R graphics package.
#
#' @return a list with b + 1 elements (b = number of bootstrap repetitions). Each element is a dataframe with 100 rows and two columns:
#' \enumerate{
#' \item p predicted probabilities.
#' \item pc calibrated probabilities.
#' }
#' The 100 rows of each dataframe stem from the default number of evaluations from the function \code{pmcalibration::pmcalibration}, i.e., generating 100 equally spaced intervals between the minimum and the maximum estimated probability. Furthermore, the default smoothing function \code{mgcv::gam} (generalized additive model) is used to calibrate the probabilities \code{p}, which produces the column \code{pc}.
#' See also Riley and Collins, 2023, section 3.2.2.
#
#' @author Stephen Rhodes (see \strong{Details})
#
#' @importFrom pmcalibration pmcalibration
#' @importFrom pminternal cal_defaults get_stability
#
#' @examples
#' # See the accompanying R script and this package's vignette,
#' # section Calibration stability.
#
#' @references
#'
#' \insertRef{pmcalibration2023}{predictAUDPsyCoLaus}
#'
#' \insertRef{pminternal2024}{predictAUDPsyCoLaus}
#'
#' \insertRef{riley2023stability}{predictAUDPsyCoLaus}
#'
#' \insertRef{collins2024evaluation}{predictAUDPsyCoLaus}
#
#' @export
#
calibration_stability1 <- function(x) {
    stabil <- pminternal::get_stability(x)
    y <- stabil$y
    stabil <- stabil$stability
    calib_args <- pminternal::cal_defaults()
    calib_args[["eval"]] <- 100
    calib_args[["p"]] <- stabil[,1]
    calib_args[["y"]] <- y
    calib_args[["plot"]] <- FALSE
    curves <- apply(stabil, 2, function(p) {
        calib_args[["p"]] <- p
        calib_args[["y"]] <- y
        cc <- do.call(pmcalibration::pmcalibration, calib_args)
        data.frame(p = cc$plot$p, pc = cc$plot$p_c_plot)
    })
    return(curves)
}
