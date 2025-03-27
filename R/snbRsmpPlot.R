#' Plot recalibration data.
#
#' @description Plot the extracted standardized net benefit results.
#
#' @param data Dataset which provides the required columns (see \strong{Details}).
#
#' @details Dataset must contain the columns 'snbOrig', 'snbStdRecal', 'snbConstRecal', and 'oneSEfromMax'; this dataset is returned by the function \code{snbVals}.
#
#' @return a ggplot2 visualization with three facet windows
#' \enumerate{
#' \item facet 1 shows 
#' \item facet 2 shows
#' \item facet 3 
#' }
#
#' @author Marcel Miché
#
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes facet_wrap geom_line
#' @importFrom tidyr pivot_longer
#
#' @examples
#' # This function is used by its wrapper function \code{snbVals}.
#
#' @references
#' 
#' \insertRef{tidyr2024}{predictAUDPsyCoLaus}
#' 
#' \insertRef{ggplot22016}{predictAUDPsyCoLaus}
#
#' @export
#
snbRsmpPlot <- function(data=NULL) {
    if(any(!c("snbOrig", "snbStdRecal", "snbConstRecal", "rsmp") %in% colnames(data))) {
        stop("Column names must include these names: snbOrig, snbStdRecal, snbConstRecal, rsmp.")
    }
    # Prepare data for plotting:
    data1 <- data[,c("snbOrig", "snbStdRecal", "snbConstRecal", "rsmp")] %>% 
        tidyr::pivot_longer(
            cols = !rsmp,
            names_to = "snbSource",
            values_to = "snb"
        )
    # 
    data1$snbSource <- factor(data1$snbSource,
                              levels=c("snbOrig", "snbStdRecal", "snbConstRecal"))
    data1$snbMax <- rep(data$snbMax, each = 3)
    
    return(
        ggplot(data=data1, aes(x=rsmp, y=snb, color = snbSource)) +
            geom_line() +
            facet_wrap(~snbSource)
    )
}
