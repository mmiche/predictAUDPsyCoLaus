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
visualizeMultipleDCA <- function(dcaLs = NULL, dcaSelectType = NULL) {

    selectedTypes <- c("mnci", "mnminmax", "mnq1q3")
    if(length(dcaSelectType) != 1 || all((selectedTypes %in% dcaSelectType) == FALSE)) {
        stop("Argument dcaSelectedType must be one of these three options: mnci, mnminmax, mnq1q3.")
    }
    
    dcaAllModels <- dplyr::bind_rows(dcaLs$tableDCA)
    listNames <- unique(dcaAllModels$model)
    
    nbWideLs <- smryModLs <- list()
    nbMean <- c()
    for(m in listNames) {
        nbWideLs[[m]] <- dcaAllModels[dcaAllModels$model==m,]
        smryModLs[[m]] <- nbWideLs[[m]]  %>%
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
        nbMean <- c(nbMean, smryModLs[[m]]$Mean)
    }
    
    allDCA <- dcaLs$plotDCA[[1]]
    insertMean <- (allDCA$label %in% c("Treat all", "Treat none")) | allDCA$threshold == 0
    # print(allDCA, n=nrow(allDCA))
    allDCA$net_benefit[!insertMean] <- nbMean
    
    selectedThresholds <- unique(dcaAllModels$thrsh)
    selectedThresholds_len <- length(selectedThresholds)
    
    mnSpanLs <- list()
    for(m in listNames) {
        wLs <- list()
        if(dcaSelectType==selectedTypes[1]) {
            #
            for(i in selectedThresholds) {
                idx <- nbWideLs[[m]]$thrsh == i
                wLs[[paste0("threshold",i)]] <- mysml::mnci(x=nbWideLs[[m]]$nbModel[idx])
            }
            
        } else if(dcaSelectType==selectedTypes[2]) {
            #
            for(i in selectedThresholds) {
                idx <- nbWideLs[[m]]$thrsh == i
                wLs[[paste0("threshold",i)]] <- mysml::mnminmax(x=nbWideLs[[m]]$nbModel[idx])
            }
            
        } else if(dcaSelectType==selectedTypes[3]) {
            #
            for(i in selectedThresholds) {
                idx <- nbWideLs[[m]]$thrsh == i
                wLs[[paste0("threshold",i)]] <- mysml::mnq1q3(x=nbWideLs[[m]]$nbModel[idx])
            }
        }
        mnSpanLs[[m]] <- wLs
    }
    
    allNone <- allDCA$label %in% c("Treat all", "Treat none")
    mdls <- as.character(allDCA$label[!allNone])
    
    mergeLs <- list()
    for(m in listNames) {
        mDf <- dplyr::bind_rows(mnSpanLs[[m]])
        mDf$threshold <- selectedThresholds
        mDf$model <- m
        mergeLs[[m]] <- mDf
    }
    
    uiDf <- dplyr::bind_rows(mergeLs)
    uiDf$threshold <- as.factor(uiDf$threshold)
    
    lenAllNone <- length(which(allNone))
    mnAppend <- lbAppend <- ubAppend <- rep(NA, times=lenAllNone)
    
    for(m in listNames) {
        idx_m <- which(uiDf$model %in% m)
        mnAppend <- c(mnAppend, NA, uiDf$mn[idx_m])
        lbAppend <- c(lbAppend, NA, uiDf[[2]][idx_m])
        ubAppend <- c(ubAppend, NA, uiDf[[3]][idx_m])
    }
    allDCA$mn <- mnAppend
    allDCA[,colnames(uiDf)[2]] <- lbAppend
    allDCA[,colnames(uiDf)[3]] <- ubAppend
    # print(allDCA, n=nrow(allDCA))
    
    outLs <- smryModLs
    outLs[["allDCA"]] <- allDCA
    names(outLs)[-length(outLs)] <- paste0(listNames, "NB")
    return(outLs)
}
