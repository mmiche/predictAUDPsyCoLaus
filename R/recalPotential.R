#' Recalibration potential.
#
#' @description Compute recalibration potential.
#
#' @param data Dataset which provides the required columns (see \strong{Details}).
#
#' @details Dataset must contain the columns 'snbOrig', 'snbStdRecal', 'snbConstRecal', and 'oneSEfromMax'; this dataset is returned by the function \code{snbVals}.
#
#' @return a numeric vector with the two values 0 (= no recalibration method required) and 1 (= some recalibration method is indicated).
#
#' @author Marcel Miché
#
#' @examples
#' # See the accompanying R script and this package's vignette,
#' # section Recalibration: Potential for improved net benefit?
#
#' @export
#
recalPotential <- function(data=NULL) {
    
    apply(data[,-1], 1, function(x) {
        
        if(
            x["snbOrig"]<=x["oneSEfromMax"]
            &
            any(
                c(
                    x["snbStdRecal"]>x["oneSEfromMax"],
                    x["snbConstRecal"]>x["oneSEfromMax"]
                )
            )
        ) {
            1
        } else {
            0
        }
    })
}
