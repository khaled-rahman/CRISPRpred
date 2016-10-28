#' Making Formula for Learning
#'
#' This function takes a list of features to make a suitable formula. For example, it creates a formula Y~X1+X2+X3 from a list of features ('X1','X2','X3','Y')
#' @param featurelist a list of strings
#' @return a formula
#' @export
#' @examples 
#' featurelist = c('X1', 'X2', 'X3', 'Y')
#' formula = featureformula(featurelist)
#' formula
featureformula = function(featurelist) {
  predictionfeature = featurelist[length(featurelist)]
  righthand =
    paste(featurelist[-length(featurelist)], collapse = "+")
  paste0(predictionfeature,"~", righthand)
}
