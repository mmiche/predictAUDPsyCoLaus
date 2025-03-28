#' Run bootstrap performance optimism estimation, applied to DCA.
#
#' @description Apply the bootstrap evaluation of the model DCA performance.
#
#' @param B number of bootstrap resamples to run (we set B = 500).
#
#' @param data Dataset which provides the required columns (see \strong{Details}).
#
#' @param outcome Character string. Name of the outcome variable, in this study it was 'newAud' (new onset of alcohol use disorder).
#
#' @param thresholds Vector with numeric values. Selected range of reasonable threshold probabilities, we used: .01, .02, .03, .04, and .05.
#
#' @details Dataset must contain the outcome and all eight predictor variables. This function is a modification and extension of the function \code{boot_optimism} of the R package \code{pminternal}.
#' Marcel Miché modified and extended the original code. Enter predictAUDPsyCoLausDev::boot_optimismDCA in the R console to see the raw code, including the code that I outcommented.
#
#' @return a data.frame with three rows (names: Apparent, Optimism, and Corrected) and as many columns as threshold probabilities were selected:
#' \enumerate{
#' \item Apparent Mean apparent net benefit, given the specifiied threshold probability, across all 500 bootstrap versions of the original data.
#' \item Optimism Mean net benefit difference between the net benefit of the apparent and the cross-validated model performance, for the specified threshold probability.
#' \item Corrected Apparent net benefit minus optimistic net benefit.
#' }
#
#' @author Stephen Rhodes (see \strong{Details})
#
#' @importFrom parallel clusterExport makeCluster stopCluster
#' @importFrom pbapply pbapply
#' @importFrom stats binomial family glm predict
#' @importFrom stats formula
#
#' @examples
#' # See the accompanying R script and this package's vignette,
#' # section Optimism corrected net benefit.
#
#' @references
#'
#' \insertRef{pbapply2023}{predictAUDPsyCoLaus}
#'
#' \insertRef{R2024}{predictAUDPsyCoLaus}
#
#' @export
#
# Remove these original function arguments (reason: not needed): score_fun, method.
# Add new argument: thresholds
boot_optimismDCA <- function (data, outcome="newAud", B = 200, thresholds = NULL) {
    # dots <- list(...)
    # method <- match.arg(method)
    # if (missing(score_fun)) {
    #     score_fun <- pminternal::score_binary
    # }
    
    # --------------------------------------
    # This is a minimally modified version of model_fun and of
    # pred_fun, see Examples of function pminternal::boot_optimism.
    # pminternal package version 0.0.1
    model_fun <- function(data){
        fmla <- formula(paste(outcome, "~."))
        glm(fmla, data=data, family="binomial")
    }
    
    pred_fun <- function(model, data){
        predict(model, newdata=data, type="response")
    }
    # --------------------------------------
    
    method <- "boot"
    fit <- model_fun(data = data)
    p_app <- pred_fun(model = fit, data = data)
    y <- data[[outcome]]

    dc_app <- dca_fun(y=y, p=p_app, thresholds = thresholds)

    # score_app <- score_fun(y = y, p = p_app, ...)
    n <- nrow(data)
    indices <- matrix(integer(1), nrow = n, ncol = B)
    W <- matrix(TRUE, nrow = n, ncol = B)
    for (i in seq(B)) {
        indices[, i] <- s <- sample(n, replace = TRUE)
        W[s, i] <- FALSE
    }
    # # This code chunk is not needed, just use method boot.
    # if (method == ".632") {
    #     nomit <- apply(W, 1, sum)
    #     if (any(nomit == 0)) {
    #         stop("not every observation omitted at least once ",
    #              "in bootstrap samples.\nRe--run with larger B")
    #     }
    #     wt <- 1 - (1/B - apply(W/nomit, 2, sum)/n)
    # }
    # else wt <- NULL
    wt <- NULL
    # if ("cores" %in% names(dots)) {
    #     cores <- dots[["cores"]]
    # }
    # else {
    #     cores <- 1
    # }
    cores <- 1
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl,
                            varlist =
                                c("B", "data", "indices", "wt", "method",
                                  "model_fun", "pred_fun", "dca_fun"),
                            envir = environment())

    S <- pbapply::pblapply(seq(B), function(i) {
        data_i <- data[indices[, i], ]
        model_i <- model_fun(data = data_i)
        # if (method == "boot") {
            p_orig <- pred_fun(model = model_i, data = data)
            dca_orig <- dca_fun(y=y, p=p_orig, thresholds = thresholds)

            p_boot <- pred_fun(model = model_i, data = data_i)
            dca_boot <- dca_fun(y=data_i[[outcome]], p=p_boot, thresholds = thresholds)

            # score_orig <- score_fun(y = data[[outcome]], p = p_orig,
            #                         ...)
            # score_boot <- score_fun(y = data_i[[outcome]], p = p_boot,
            #                         ...)
            # optimism <- score_boot - score_orig
            optimism <- dca_boot$net_benefit - dca_orig$net_benefit
        # }
        # # This code chunk is not needed, just use method boot.
        # else {
        #     p_orig <- score_orig <- NULL
        #     data_omit_i <- data[-indices[, i], ]
        #     p_omit <- pred_fun(model = model_i, data = data_omit_i,
        #                        ...)
        #     score_omit <- score_fun(y = data_omit_i[[outcome]],
        #                             p = p_omit, ...)
        #     optimism <- 0.632 * (score_app - score_omit * wt[i])
        # }
        # list(optimism = optimism, p_orig = p_orig, score_orig = score_orig)
        list(optimism = optimism)
    }, cl = cl)
    parallel::stopCluster(cl)
    opt <- do.call(rbind, lapply(S, function(x) x$optimism))
    opt <- apply(opt, 2, mean)
    bcorr <- dc_app$net_benefit - opt
    # # This code chunk is not needed.
    # if (method == "boot") {
    #     simple_boot <- do.call(rbind, lapply(S, function(x) x$score_orig))
    #     simple_boot <- apply(simple_boot, 2, mean)
    #     stability <- do.call(cbind, lapply(S, function(x) x$p_orig))
    #     stability <- cbind(p_app = p_app, stability)
    # }
    # else {
    #     simple_boot <- stability <- NULL
    # }
    # out <- list(apparent = dc_app$net_benefit, optimism = opt, corrected = bcorr,
    #             simple = simple_boot, stability = stability, y = y, method = method)
    # class(out) <- "internal_boot"
    # return(out)

    dcaOptim <- data.frame(matrix(data=c(dc_app$net_benefit,
                                         opt,
                                         bcorr), nrow=3, byrow = TRUE))
    colnames(dcaOptim) <- paste0("pt", thresholds)
    rownames(dcaOptim) <- c("Apparent", "Optimism", "Corrected")

    return(dcaOptim)
}
