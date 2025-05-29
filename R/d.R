#' A data set, used in predictAUDPsyCoLaus.
#'
#' This dummy dataset d has been simulated.
#'
#' @format A data.frame with 3654 rows and 9 columns:
#' \itemize{
#'   \item firstAud (binary outcome; 0 = no event observed, 1 = event observed).
#'   \item ADAPU2 (binary predictor; 0, 1).
#'   \item inactivity (binary predictor; 0, 1).
#'   \item MDDPD2 (binary predictor; 0, 1).
#'   \item Sex (binary predictor; 0, 1).
#'   \item MARIE (binary predictor; 0, 1).
#'   \item iSES15 (categorical predictor; minimum 1, maximum 5).
#'   \item smokingstatus (categorical predictor; minimum 0, maximum 2).
#'   \item Week_ALC_type6 (categorical predictor; minimum 1, maximum 6).
#' }
#' Note: The only two columns which have the same distribution as in the original data, are the outcome \code{firstAud} (115 outcome cases) and the predictor smokingstatus (category 0 = 1787, category 1 = 1221, category 2 = 646).
#'
#' @docType data
#' @keywords simulated dataset
#' @name d
#'
#' @usage data(d)
#' @examples
#' # Display the structure of the data set in the console
#' str(d)
"d"
