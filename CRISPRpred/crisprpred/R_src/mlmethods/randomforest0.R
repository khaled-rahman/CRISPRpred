#' Random Forest
#'
#' This function takes a list of learning features, dataset and no. of trees. It calculates prediction score based on randomForest randomforest model.
#' @param featurelist a list of features. last name will indicate the value to be predicted.
#' @param featuredata a sample dataset containing all features
#' @param trees number of trees that will be built.
#' @return predictionsS prediction values
#' @export
#' @examples
#' featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' #suppose we have a file as '../crisprpred/data-raw/sample.csv' and current directory is set to '../crisprpred'
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' randomforest(featurelist,data)

randomforest0 = function(featurelist, featuredata,trees = 500) {
  fformula = featureformula(featurelist)
  modelS = randomForest(
    as.formula(fformula), featuredata, proximity = FALSE, importance = TRUE, ntree = trees
  )
  predictionsS = predict(modelS, featuredata)
  
  return(predictionsS)
}
