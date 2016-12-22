#' MARS Regression
#'
#' This function takes mars regression formula and dataset. Now, it outputs predicted values.
#' @param featurelist a list of features
#' @param featuredata provided dataset
#' @return predictionsE predicted values
#' @export 
#' @examples
#' featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' mars(featurelist,data)
mars = function(featurelist, featuredata){
  fformula =featureformula(featurelist)
  modelE = earth(as.formula(fformula), featuredata, nfold = 10)
  predictionsE = predict(modelE, featuredata)
  return(predictionsE)
}
