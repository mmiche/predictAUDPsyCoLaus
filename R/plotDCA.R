#' Plot DCA results.
#
#' @description Function that outputs plotted decision curve analysis results.
#
#' @param allDCA The output 'allDCA' from function \code{visualizeMultipleDCA}.
#
#' @param bothModels Boolean value. TRUE, if logistic regression and random forest shall both be shown in the plot, FALSE, if only logistic regression.
#
#' @return a list with two elements:
#' \enumerate{
#' \item dcaPlot1 Decision curve analysis plot, showing only the mean net benefit curve.
#' \item dcaPlot2 In addition to dcaPlot1, this plot also shows the interval across all cross-validated net benefit curves.
#' }
#' There are three options of intervals for dcaPlot2: mnci = mean and lower to upper 95 percent CI, mnminmax = mean and minimum to maximum, mnq1q3 = mean and first to third quantile.
#
#' @author Marcel Miché
#
#' @importFrom ggplot2 ggplot aes coord_cartesian dup_axis element_blank element_rect element_text geom_errorbar geom_line geom_point guide_legend guides labs position_dodge scale_colour_manual scale_x_continuous theme ylab ylim
#' @importFrom rlang .data
#
#' @examples
#' # See the accompanying R script and this package's vignette,
#' # section 1. Decision curve analysis (DCA), performance measure:
#' # Net benefit (NB).
#
#' @references
#'
#' \insertRef{ggplot22016}{predictAUDPsyCoLaus}
#
#' @export
#
plotDCA <- function(allDCA = NULL, bothModels=FALSE) {

    modNames <- as.character(unique(allDCA$label))
    
    if(bothModels) {
        dca <- allDCA
        useColor <- c("black", "darkgrey", "red", "blue")
        names(useColor) <- modNames
    } else {
        dca <- allDCA[!allDCA$label %in% modNames[length(modNames)],]
        dca$label <- droplevels(dca$label)
        useColor <- c("black", "darkgrey", "red")
        names(useColor) <- modNames[-length(modNames)]
    }
    htbPlot <- function(x) paste0("1:", round((1-x)/x, digits=2))
    # Make dca plot
    dcaPlot <-
        ggplot(data=dca, aes(x=.data$threshold, y=.data$net_benefit, colour=.data$label)) +
        geom_line(aes(colour=.data$label), linewidth=.75) +
        labs(color=NULL) +
        scale_x_continuous(
            sec.axis = dup_axis(name="Harm-to-benefit ratio", labels=htbPlot)) +
        # Take control of the y-axis: How much of the negative part shall be visible?
        coord_cartesian(ylim=c(-.005, .033), xlim=c(0, .05)) +
        scale_colour_manual(values = useColor) +
        ylab(label="Net benefit") +
        theme(
            panel.background = element_blank(),
            axis.text.x=element_text(size=16),
            axis.title.x=element_text(size=16),
            axis.text.y=element_text(size=16),
            axis.title.y = element_text(size=16),
            panel.border = element_rect(color="black", fill=NA),
            legend.text = element_text(size=14),
            legend.position = "top",
            legend.title = element_blank()) +
        labs(x="Threshold probability")
    
    if(any(colnames(dca) == "lci")) {
        # Mean net benefit and 95% CI:
        # ---------------------------
        if(bothModels) {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3, position = position_dodge(width=.002)) +
                geom_errorbar(width=.003, aes(ymin=.data$lci, ymax=.data$uci), linewidth=1, position=position_dodge(width=0.002)) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        } else {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3) +
                geom_errorbar(width=.003, aes(ymin=.data$lci, ymax=.data$uci), linewidth=1) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        }
        
    } else if(any(colnames(dca) == "min")){
        # Mean and Minimum/Maximum net benefit:
        # ------------------------------------
        if(bothModels) {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3, position = position_dodge(width=.002)) +
                geom_errorbar(width=.003, aes(ymin=.data$min, ymax=.data$max), linewidth=1, position=position_dodge(width=0.002)) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        } else {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3) +
                geom_errorbar(width=.003, aes(ymin=.data$min, ymax=.data$max), linewidth=1) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        }
        
    } else if(any(colnames(dca) == "q1")) {
        # Mean and q1/q3 net benefit:
        # ------------------------------------
        if(bothModels) {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3, position = position_dodge(width=.002)) +
                geom_errorbar(width=.003, aes(ymin=.data$q1, ymax=.data$q3), linewidth=1, position=position_dodge(width=0.002)) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        } else {
            dcaPlotOverlay <-
                dcaPlot +
                geom_point(aes(y=.data$mn), size=3) +
                geom_errorbar(width=.003, aes(ymin=.data$q1, ymax=.data$q3), linewidth=1) +
                guides(color = guide_legend(override.aes = list(shape=NA)))
        }
    }
    return(list(dcaPlot1=dcaPlot, dcaPlot2=dcaPlotOverlay))
}
