#' SMV Regression
#'
#' This function takes feature list and dataset. Now, it outputs prediction score.
#' @param featurelist a list of features
#' @param featuredata provided dataset
#' @return prediction score
#' @export
#' @examples
#' featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' svmregression(featurelist,data)
svmregression = function(featurelist,featuredata) {
  fformula = featureformula(featurelist)
  modelS = svm(as.formula(fformula), featuredata, cross = kfold)
  predictionsS = predict(modelS, featuredata)
  cat("SVM predictions:", predictionsS, "\n")
  return(predictionsS)
}
