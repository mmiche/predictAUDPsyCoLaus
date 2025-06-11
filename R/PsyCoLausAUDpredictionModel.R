#' Prediction model to predict new onset alcohol use disorder.
#'
#' This prediction model was developed by using the full dataset (N = 3654) in the publication, of which this R package is part of the supplementary material.
#'
#' @format A list with 23 elements
#'
#' This list is an S3 object of the classes "glm" and "lm". Of the original fitted generalized linear model (glm), all original raw data has been removed, due to data privacy laws. That is, this prediction model can still be used with the convenient 'predict' function, which is supplied by the R stats package. Indeed, this is the whole point why we provide our prediction model, namely to enable external cross-validation with other longitudinal data (Steyerberg and Harrell, 2016).
#'
#' Instruction of how to use our prediction model in R:
#'
#' \itemize{
#'   \item Make sure your data stems from a general community adult sample. If not, consider recalibration (Van Calster et al., 2019).
#'   \item Make sure your data contains the eight predictors, which we list below.
#'   \item Make sure that the coding of all predictors concur with our coding.
#'   \item Make sure the outcome in your data is new onset alcohol use disorder, according to DSM-5.
#'   \item Before applying our prediction model to your data, rename your predictor columns, so that they match our predictor names.
#' }
#'
#' List of the eight predictors which we used to predict new-onset of alcohol use disorder (according to DSM-5):
#'
#' \enumerate{
#'   \item Sex (0 = male, 1 = female)
#'   \item iSES15 (Socio-economic status (Hollingshead (1975); 1 = lowest to 5 = highest))
#'   \item MARIE (Marital status, 0 = not married, 1 = married)
#'   \item MDDPD2 (lifetime major depressive disorder diagnosis (according to DSM-IV-TR), 0 = no, 1 = yes)
#'   \item ADAPU2 (lifetime any anxiety disorder diagnosis (includes agoraphobia, social anxiety disorder, panic disorder, and generalized anxiety disorder; according to DSM-IV-TR), 0 = no, 1 = yes)
#'   \item Week_ALC_type6 (Description below this list, 1 = least to 6 = most)
#'   \item smokingstatus (0 = never/non-regular, 1 = former, 2 = current)
#'   \item inactivity (physical inactivity; 0 = active at least once a week, 1 = active less than once a week)
#' }
#' Week_ALC_type6: Total number of consumed alcoholic drinks in a typical week (all seven days). The original raw data expressed easily comprehensible alcohol quantities, e.g., a small glas of whiskey. For example, one small glass of whiskey (strong alcohol) received the value 1, whereas a quarter of one liter received the value 6, same as one bottle of wine or six bottles of beer. We transformed these original raw measurements into standard glasses of alcohol (= 10-12 grams of pure alcohol). In detail:
#' \itemize{
#'   \item 0 to 7 = 1 = One standard glass or less.
#'   \item 8 to 14 = 2 = > 1 to 2 standard glasses.
#'   \item 15 to 21 = 3 = > 2 to 3 standard glasses.
#'   \item 22 to 28 = 4 = > 3 to 4 standard glasses.
#'   \item 29 to 35 = 5 = > 4 to 5 standard glasses.
#'   \item 36 and higher = 6 = > 5 standard glasses.
#' }
#'
#' @examples
#' # Display the prediction model's estimates in the R console:
#' cbind(coefficients(predictAUDPsyCoLaus::PsyCoLausAUDpredictionModel))
#' # Output in R console:
#' # (Intercept)    -5.35428124
#' # Sex            -0.20722125
#' # iSES15          0.13956629
#' # MARIE          -0.32800250
#' # MDDPD2          0.28228634
#' # ADAPU2         -0.04888268
#' # Week_ALC_type6  0.64022013
#' # smokingstatus   0.60040634
#' # inactivity      0.10690985
#' # Take a random subset of the simulated dummy data d, which is included in
#' # this package.
#' d_subset <- d[c(5, 35, 599, 1697),]
#' # The prediction model gets loaded automatically when loading this package.
#' # Apply the prediction model to the selected subset,
#' # output = predicted probability to develop alcohol use disorder.
#'  predict(object = PsyCoLausAUDpredictionModel, newdata = d_subset,
#'  type = "response")
#' # Alternatively, compute predicted probability with x percent confidence
#' # interval (CI), e.g., x = 95.
#' # prlo: predicted log-odds
#' prlo <- predict(object = PsyCoLausAUDpredictionModel,
#' newdata = d_subset, se.fit = TRUE)
#' # Lower 95 (l95) and upper 95 (u95) percent CI:
#' l95 <- prlo$fit + qnorm(.025) * prlo$se.fit
#' u95 <- prlo$fit + qnorm(.975) * prlo$se.fit
#' # Predicted probabilities (plogis(prlo)) with 95 percent CI
#' data.frame(predProb=plogis(prlo$fit),
#' predProbLower95=plogis(l95), predProbUpper95=plogis(u95))
#'
#' @docType data
#' @keywords model
#' @importFrom Rdpack reprompt
#' @name PsyCoLausAUDpredictionModel
#'
#' @references
#' \insertRef{steyerberg2016prediction}{predictAUDPsyCoLaus}
#'
#' \insertRef{van2019calibration}{predictAUDPsyCoLaus}
#'
#' \insertRef{hollingshead1975four}{predictAUDPsyCoLaus}
NULL
