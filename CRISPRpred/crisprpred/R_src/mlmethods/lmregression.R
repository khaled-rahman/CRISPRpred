#' Linear Regression
#'
#' This function takes featurelist and dataset. Now, it creates a formula and outputs predicted values based on provided dataset.
#' @param featurelist a list of feature
#' @param featuredata provided dataset
#' @return predictionsS predicted values
#' @export
#' @examples
#' featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' lmregression(featurelist,data)
lmregression = function(featurelist, featuredata) {
  fformula = featureformula(featurelist)
  model1 = lm(as.formula(fformula), featuredata)
  predictionsS = stats::predict(model1, featuredata)
  return(predictionsS)
}
