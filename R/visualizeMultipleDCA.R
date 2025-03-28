#' Prepare data for plotDCA.
#
#' @description Function that prepares the data for the function plotDCA.
#
#' @param dcaLs The output dcaLs from function \code{computeRelevantResults} from the \code{mysml} package.
#
#' @param dcaSelectType Character string. One out of these three options (see \strong{Details}): mnci, mnminmax, mnq1q3.
#
#' @return a list with three elements:
#' \enumerate{
#' \item allDCA A dataframe needed by the function \code{plotDCA}.
#' \item logregNB A dataframe with as many rows as selected threshold probabilities. The columns contain summaries of the net benefit, i.e., the median, the mean, the minimum, the first and the third quantile, and the maximum.
#' \item rfNB Same as logregNB, only for the random forest model.
#' }
#
#' @author Marcel Miché
#
#' @importFrom stats median
#' @importFrom mysml mnci mnminmax mnq1q3
#' @importFrom dplyr bind_rows group_by summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#
#' @examples
#' # See the accompanying R script and this package's vignette,
#' # section 1. Decision curve analysis (DCA), performance measure:
#' # Net benefit (NB).
#
#' @references
#'
#' \insertRef{mysml2024}{predictAUDPsyCoLaus}
#'
#' \insertRef{dplyr2023}{predictAUDPsyCoLaus}
#'
#' \insertRef{magrittr2022}{predictAUDPsyCoLaus}
#
#' @export
#
# dcaLs = test$dcaLs; dcaSelectType = "mnminmax"
visualizeMultipleDCA <- function(dcaLs = NULL, dcaSelectType = NULL) {

    selectedTypes <- c("mnci", "mnminmax", "mnq1q3")
    if(length(dcaSelectType) != 1 || all((selectedTypes %in% dcaSelectType) == FALSE)) {
        stop("Argument dcaSelectedType must be one of these three options: mnci, mnminmax, mnq1q3.")
    }

    dcaAllModels <- dplyr::bind_rows(dcaLs$tableDCA)
    nbWideLogreg <- dcaAllModels[dcaAllModels$model=="logreg",]
    nbWideRf <- dcaAllModels[dcaAllModels$model=="randomForest",]

    logregNB <- nbWideLogreg %>%
        dplyr::group_by(.data$thrsh) %>%
        dplyr::summarise(
            Median=median(.data$nbModel),
            Mean=mean(.data$nbModel),
            Min=min(.data$nbModel),
            qu1=as.numeric(summary(.data$nbModel)[2]),
            lci=as.numeric(mysml::mnci(.data$nbModel)[2]),
            uci=as.numeric(mysml::mnci(.data$nbModel)[3]),
            qu3=as.numeric(summary(.data$nbModel)[5]),
            Max=max(.data$nbModel)
        )

    rfNB <- nbWideRf %>%
        dplyr::group_by(.data$thrsh) %>%
        dplyr::summarise(
            Median=median(.data$nbModel),
            Mean=mean(.data$nbModel),
            Min=min(.data$nbModel),
            qu1=as.numeric(summary(.data$nbModel)[2]),
            lci=as.numeric(mysml::mnci(.data$nbModel)[2]),
            uci=as.numeric(mysml::mnci(.data$nbModel)[3]),
            qu3=as.numeric(summary(.data$nbModel)[5]),
            Max=max(.data$nbModel)
        )

    allDCA <- dcaLs$plotDCA[[1]]
    allDCA$net_benefit[c(14:18,20:24)] <- c(logregNB$Mean, rfNB$Mean)

    logregMnSpan <- list()
    rfMnSpan <- list()

    if(dcaSelectType==selectedTypes[1]) {
        #
        for(i in unique(nbWideLogreg$thrsh)) {
            idx <- nbWideLogreg$thrsh == i
            logregMnSpan[[paste0("threshold",i)]] <- mysml::mnci(x=nbWideLogreg$nbModel[idx])
        }

        for(i in unique(nbWideRf$thrsh)) {
            idx <- nbWideRf$thrsh == i
            rfMnSpan[[paste0("threshold",i)]] <- mysml::mnci(x=nbWideRf$nbModel[idx])
        }
    } else if(dcaSelectType==selectedTypes[2]) {
        #
        for(i in unique(nbWideLogreg$thrsh)) {
            idx <- nbWideLogreg$thrsh == i
            logregMnSpan[[paste0("threshold",i)]] <- mysml::mnminmax(x=nbWideLogreg$nbModel[idx])
        }

        for(i in unique(nbWideRf$thrsh)) {
            idx <- nbWideRf$thrsh == i
            rfMnSpan[[paste0("threshold",i)]] <- mysml::mnminmax(x=nbWideRf$nbModel[idx])
        }
    } else if(dcaSelectType==selectedTypes[3]) {
        #
        for(i in unique(nbWideLogreg$thrsh)) {
            idx <- nbWideLogreg$thrsh == i
            logregMnSpan[[paste0("threshold",i)]] <- mysml::mnq1q3(x=nbWideLogreg$nbModel[idx])
        }
        for(i in unique(nbWideRf$thrsh)) {
            idx <- nbWideRf$thrsh == i
            rfMnSpan[[paste0("threshold",i)]] <- mysml::mnq1q3(x=nbWideRf$nbModel[idx])
        }
    }

    selectedThresholds <- unique(dcaAllModels$thrsh)
    selectedThresholds_len <- length(selectedThresholds)

    logreg_ui <- dplyr::bind_rows(logregMnSpan)
    logreg_ui$threshold <- selectedThresholds
    logreg_ui$model <- "logreg"

    rf_ui <- dplyr::bind_rows(rfMnSpan)
    rf_ui$threshold <- selectedThresholds
    rf_ui$model <- "randomForest"

    uiDf <- dplyr::bind_rows(logreg_ui, rf_ui)
    uiDf$model <- c(rep("Logistic regression", times=selectedThresholds_len),
                    rep("Random forest", times=selectedThresholds_len))
    uiDf$model <- as.factor(uiDf$model)
    uiDf$threshold <- as.factor(uiDf$threshold)

    idxLR <- which(uiDf$model %in% "Logistic regression")
    idxRF <- which(uiDf$model %in% "Random forest")

    lenAllNone <- length(which(allDCA$label %in% c("Treat all", "Treat none")))
    allDCA$mn <- c(rep(NA, times=lenAllNone), NA, uiDf$mn[idxLR], NA, uiDf$mn[idxRF])

    # ---------------
    # Append selected type
    # ---------------
    if(dcaSelectType==selectedTypes[1]) {

        allDCA$lci <- c(rep(NA, times=lenAllNone), NA, uiDf$lci[idxLR], NA, uiDf$lci[idxRF])
        allDCA$uci <- c(rep(NA, times=lenAllNone), NA, uiDf$uci[idxLR], NA, uiDf$uci[idxRF])

    } else if(dcaSelectType==selectedTypes[2]){

        allDCA$min <- c(rep(NA, times=lenAllNone), NA, uiDf$min[idxLR], NA, uiDf$min[idxRF])
        allDCA$max <- c(rep(NA, times=lenAllNone), NA, uiDf$max[idxLR], NA, uiDf$max[idxRF])

    } else if(dcaSelectType==selectedTypes[3]) {

        allDCA$q1 <- c(rep(NA, times=lenAllNone), NA, uiDf$q1[idxLR], NA, uiDf$q1[idxRF])
        allDCA$q3 <- c(rep(NA, times=lenAllNone), NA, uiDf$q3[idxLR], NA, uiDf$q3[idxRF])

    }
    return(list(allDCA=allDCA, logregNB=logregNB, rfNB=rfNB))
}
