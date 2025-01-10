#' A data set, used in predictAUDPsyCoLaus.
#'
#' This dummy dataset d has been simulated.
#'
#' @format A data.frame with 3629 rows and 9 columns:
#' \itemize{
#'   \item newAud (binary outcome; 0 = no event observed, 1 = event observed).
#'   \item ADAPU2 (binary predictor; 0, 1).
#'   \item inactivity (binary predictor; 0, 1).
#'   \item MDDPD2 (binary predictor; 0, 1).
#'   \item Sex (binary predictor; 0, 1).
#'   \item MARIE (binary predictor; 0, 1).
#'   \item iSES15 (categorical predictor; minimum 1, maximum 5).
#'   \item smokingstatus (categorical predictor; minimum 0, maximum 2).
#'   \item Week_ALC_type6 (categorical predictor; minimum 1, maximum 6).
#' }
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
