#' Recalibration plot data.
#
#' @description Extract standardized net benefit results of the recalibration plot.
#
#' @param p Vector of risk score values (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).
#
#' @param p.const Vector of risk score values after constrained logistic recalibration (argument added by us).
#
#' @param p.std Vector of risk score values after standard logistic recalibration (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).
#
#' @param r Numeric value. Clinically relevant risk threshold (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).
#
#' @param risk.model.std Boolean value (default = TRUE). Plot standard error bars for sNB of plotted risk models (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).
#
#' @param stdErrThresh Numeric value (default = 1). Indicates how many standard errors line drawn below the maximum of the sNB curve should be (see Arguments of \code{ClinicalUtilityRecal::snbRecalPlot}).
#
#' @param y Vector of integer values that are either 0 (= outcome not observed) or 1 (= outcome observed).
#
#' @details Marcel Miché slightly modified the function snbRecalPlot from the package \code{ClinicalUtilityRecal}, in order to be able to collect the results in numeric form.
#
#' @return a named vector with five elements:
#' \enumerate{
#' \item snbMax The maximum possible standardized net benefit (snb), given the specified threshold probability.
#' \item snbOrig The snb which is based on the logistic regression model (no recalibration).
#' \item snbStdRecal The snb which is based on the standard recalibration method by Cox (1954).
#' \item snbConstRecal The snb which is based on the contrained logistic recalibration method by Mishra et al. (2022).
#' \item oneSEfromMax One standard error away from snbMax.
#' }
#' Note: The one standard error distance from snbMax is shown as dashed horizontal line in Figures 1, 4, and 5 in Mishra et al. (2022).
#
#' @author Anu Mishra (see \strong{Details})
#
#' @importFrom ClinicalUtilityRecal nb
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
snbRecalPlotNum <- function (p, p.std, p.const, y, r, stdErrThresh = 1, risk.model.std = TRUE) {
    
    # --------------------------------------
    # This is a complete copy of ClinicalUtilityRecal:::snb.t
    # and of ClinicalUtilityRecal:::snbVar.tmax
    # ClinicalUtilityRecal package version 0.1.0
    snb.t <- function (par, y, p, r) {
        TPR <- mean(p[y == 1] > par)
        FPR <- mean(p[y == 0] > par)
        y.bar <- mean(y)
        sNB <- (TPR - ((r/(1 - r)) * ((1 - y.bar)/y.bar) * FPR))
        return(sNB)
    }
    
    snbVar.tmax <- function (tVec, y, p, r) {
        if (length(p) != length(y)) 
            stop("length of p does not match length y")
        if (any(tVec < 0) | any(tVec > 1)) 
            stop("t must be between 0 and 1")
        n <- length(p)
        snbVarVec <- rep(NA, length(tVec))
        for (j in 1:length(tVec)) {
            p11 <- mean(y == 1 & p >= tVec[j])
            p10 <- mean(y == 0 & p >= tVec[j])
            p01 <- mean(y == 1 & p < tVec[j])
            p00 <- mean(y == 0 & p < tVec[j])
            k <- r/(1 - r)
            sum <- (p10 * (p10 + p01) * k^2) + p11 * (p01 + (k^2 * 
                                                                 p10))
            var <- sum/(p11 + p01)^3
            snbVarVec[j] <- sqrt(var/n)
        }
        snbVar.tmax <- snbVarVec
        return(snbVar.tmax)
    }
    # --------------------------------------
    
    t.vec <- seq(0, 1, 5e-04)
    sNB <- cbind(t.vec, NA)
    for (i in 1:length(t.vec)) {
        pick.t <- t.vec[i]
        sNB[i, 2] <- snb.t(par = pick.t, y = y, p = p, r = r)
    }
    t.max <- NULL
    t.max$maximum <- sNB[which.max(sNB[, 2]), 1]
    t.max$objective <- sNB[which.max(sNB[, 2]), 2]
    sNB.max.se <- snbVar.tmax(tVec = t.max$maximum, y = y, p = p, 
                                                     r = r)
    upp <- t.max$objective + stdErrThresh * sNB.max.se
    low <- t.max$objective - stdErrThresh * sNB.max.se
    snb.orig <- ClinicalUtilityRecal::nb(y = y, p = p, r = r)$snb
    snb.recal <- ClinicalUtilityRecal::nb(y = y, p = p.std, r = r)$snb
    
    # ----------------------------------------------
    # Not part of Mishra's original snbRecalPlot function.
    snb.const <- ClinicalUtilityRecal::nb(y = y, p = p.const, r = r)$snb
    # ----------------------------------------------
    
    # if (is.null(ylim)) {
    #     ylim = c(min(c(snb.orig, snb.recal, 0)), max(c(snb.orig, 
    #                                                    snb.recal, 0.8)))
    # }
    # plot(sNB[, 1], sNB[, 2], type = "l", col = "black", lwd = 2, 
    #      ylim = ylim, xlab = "Threshold (t) for Decision Rule", 
    #      ylab = "sNB", main = titlePlot)
    snb.t.orig <- sNB[which.min(abs(snb.orig - sNB[, 2])), 1]
    sNB.t.orig.se <- snbVar.tmax(tVec = snb.t.orig, y = y, p = p, 
                                                        r = r)
    snb.t.std <- sNB[which.min(abs(snb.recal - sNB[, 2])), 1]
    sNB.t.std.se <- snbVar.tmax(tVec = snb.t.std, y = y, p = p, 
                                                       r = r)
    # points(snb.t.std, sNB[which.min(abs(snb.recal - sNB[, 2])), 
    #                       2], col = "blue", pch = 1, cex = 1.3, lwd = 3)
    # points(snb.t.orig, sNB[which.min(abs(snb.orig - sNB[, 2])), 
    #                        2], col = "red", pch = 1, cex = 1.2, lwd = 3)
    # abline(h = low, lwd = 1, col = "black", lty = c(2, 3, 4))
    # if (risk.model.std == TRUE) {
    #     arrows(x0 = snb.t.std, y0 = sNB[which.min(abs(snb.recal - 
    #                                                       sNB[, 2])), 2] - sNB.t.std.se, x1 = snb.t.std, y1 = sNB[which.min(abs(snb.recal - 
    #                                                                                                                                 sNB[, 2])), 2] + sNB.t.std.se, angle = 90, length = 0.1, 
    #            lwd = 1.5, code = 3, col = "blue")
    #     arrows(x0 = snb.t.orig, y0 = sNB[which.min(abs(snb.orig - 
    #                                                        sNB[, 2])), 2] - sNB.t.orig.se, x1 = snb.t.orig, 
    #            y1 = sNB[which.min(abs(snb.orig - sNB[, 2])), 2] + 
    #                sNB.t.orig.se, angle = 90, length = 0.1, lwd = 1.5, 
    #            code = 3, col = "red")
    # }
    # legend("topleft", paste("Max(sNB) =", round(t.max$objective, 
    #                                             3)), bty = "n")
    # legend("topright", c("Orig Risk Model", "Std. Log. Recal. Risk Model", 
    #                      paste(stdErrThresh, "Std Err from Maximum")), col = c("red", 
    #                                                                            "blue", "black"), pch = c(1, 1, NA), lwd = c(1.5, 1.5), 
    #        lty = c(NA, NA, 2), bty = "n")
    
    return(c(snbMax=t.max$objective, snbOrig=snb.orig, snbStdRecal=snb.recal, snbConstRecal=snb.const, oneSEfromMax=low))
}
