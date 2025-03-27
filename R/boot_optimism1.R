#' Run bootstrap performance optimism estimation.
#
#' @description Apply the bootstrap evaluation of the model performance.
#
#' @param B number of bootstrap resamples to run (we set B = 500).
#
#' @param data Dataset which provides the required columns (see \strong{Details}).
#
#' @param outcome Character string. Name of the outcome variable, in this study it was 'newAud' (new onset of alcohol use disorder).
#
#' @details Dataset must contain the outcome and all eight predictor variables. This function is a modification of the function \code{boot_optimism} of the R package \code{pminternal}.
#' Marcel Miché modified the original code. Enter predictAUDPsyCoLausDev::boot_optimism1 in the R console to see the raw code, including the code that I outcommented.
#
#' @return a data.frame with as many rows as the original data and with as many columns, as the function's argument B was set to, e.g., 500. Each column contains predicted probabilities. They were produced by training the logistic regression model with a bootstrapped version of the original data, which then was cross-validated with the original data. The predicted probabilities stem from this cross-validation with the original data.
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
#' # section 500 bootstrap prediction performance results.
#
#' @references
#'
#' \insertRef{pbapply2023}{predictAUDPsyCoLaus}
#'
#' \insertRef{R2024}{predictAUDPsyCoLaus}
#
#' @export
#
boot_optimism1 <- function (data, outcome="newAud", B = 200) {
    # dots <- list(...)
    # method <- match.arg(method)
    # if (missing(score_fun)) {
    #     score_fun <- score_binary
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
    
    fit <- model_fun(data = data)
    p_app <- pred_fun(model = fit, data = data)
    y <- data[[outcome]]
    # score_app <- score_fun(y = y, p = p_app, ...)
    n <- nrow(data)
    indices <- matrix(integer(1), nrow = n, ncol = B)
    W <- matrix(TRUE, nrow = n, ncol = B)
    for (i in seq(B)) {
        indices[, i] <- s <- sample(n, replace = TRUE)
        W[s, i] <- FALSE
    }
    # if (method == ".632") {
    #     nomit <- apply(W, 1, sum)
    #     if (any(nomit == 0)) {
    #         stop("not every observation omitted at least once ", 
    #              "in bootstrap samples.\nRe--run with larger B")
    #     }
    #     wt <- 1 - (1/B - apply(W/nomit, 2, sum)/n)
    # }
    # else wt <- NULL
    # if ("cores" %in% names(dots)) {
    #     cores <- dots[["cores"]]
    # }
    # else {
    #     cores <- 1
    # }
    cores <- 1
    wt <- NULL
    method <- "boot"
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl, varlist = c("B", "data", "indices", 
                                            "wt", "method", "model_fun", "pred_fun"),#, "score_fun"), 
                            envir = environment())
    S <- pbapply::pblapply(seq(B), function(i) {
        data_i <- data[indices[, i], ]
        # model_i <- model_fun(data = data_i, ...)
        model_i <- model_fun(data = data_i)
        # if (method == "boot") {
            # p_orig <- pred_fun(model = model_i, data = data, 
            #                    ...)
            p_orig <- pred_fun(model = model_i, data = data)
            # p_boot <- pred_fun(model = model_i, data = data_i, 
            #                    ...)
            # score_orig <- score_fun(y = data[[outcome]], p = p_orig, 
            #                         ...)
            # score_boot <- score_fun(y = data_i[[outcome]], p = p_boot, 
            #                         ...)
            # optimism <- score_boot - score_orig
        # }
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
        list(p_orig = p_orig)
    }, cl = cl)
    parallel::stopCluster(cl)
    # opt <- do.call(rbind, lapply(S, function(x) x$optimism))
    # opt <- apply(opt, 2, mean)
    # bcorr <- score_app - opt
    # if (method == "boot") {
    #     simple_boot <- do.call(rbind, lapply(S, function(x) x$score_orig))
    #     simple_boot <- apply(simple_boot, 2, mean)
    #     stability <- do.call(cbind, lapply(S, function(x) x$p_orig))
    #     stability <- cbind(p_app = p_app, stability)
    # }
    # else {
    #     simple_boot <- stability <- NULL
    # }
    # out <- list(apparent = score_app, optimism = opt, corrected = bcorr, 
    #             simple = simple_boot, stability = stability, y = y, method = method)
    # class(out) <- "internal_boot"
    # return(out)
    out <- as.data.frame(S)
    return(out)
}
