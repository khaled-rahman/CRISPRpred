#' Random Forest
#'
#' This function takes a list of learning features, dataset, no. of trees and learning rate. It calculates prediction score based on h20 randomforest model.
#' @param featurelist a list of features. last name will indicate the value to be predicted.
#' @param featuredata a sample dataset containing all features
#' @param trees number of trees that will be built. default value is 50.
#' @param learningrate a fractional sampling rate in random forest. Default value is 0.6.
#' @return predictionsS prediction values
#' @export
#' @examples
#' featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' #suppose we have a file as '../crisprpred/data-raw/sample.csv' and current directory is set to '../crisprpred'
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' randomforest(featurelist,data)
randomforest = function(featurelist, featuredata,trees = 50, learningrate = 0.6) {
  predict = featurelist[length(featurelist)]
  featurelist = featurelist[-length(featurelist)]
  
  h2o.init()
  inputdata.hex = as.h2o(featuredata, destination_frame = "inputdata.hex")
  randmodel = h2o.randomForest(
    x = featurelist, y = predict, training_frame = inputdata.hex, ntrees = trees, sample_rate = learningrate
  )
  predictionsS = h2o.predict(randmodel, inputdata.hex)
  return(predictionsS)
}
