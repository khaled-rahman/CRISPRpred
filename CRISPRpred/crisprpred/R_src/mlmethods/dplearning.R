#' Deep Learning
#'
#' This function takes a featurelist, dataset and learning rate. Now, it creates a deep learning model using  deeplearning function of h2o package and outputs predicted values based on the model.
#' @param featurelist a list of features. last name will indicate the value to be predicted.
#' @param featuredata a sample dataset containing all features
#' @param learningrate a fractional learning rate for deep learner. Default value is 0.6.
#' @return predictionsS predicted values
#' @export
#' @examples
#' featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' #suppose we have a file as '../crisprpred/data-raw/sample.csv' and current directory is set to '../crisprpred'
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' dplearning(featurelist,data)
dplearning = function(featurelist,featuredata,learningrate = 0.6) {
  predict = featurelist[length(featurelist)]
  featurelist = featurelist[-length(featurelist)]
  h2o.init()
  inputdata.hex = as.h2o(featuredata, destination_frame = "inputdata.hex")
  dpmodel = h2o.deeplearning(
    x = featurelist, y = predict, training_frame = inputdata.hex, nfolds = 10, rate = learningrate
  )
  predictionsS = h2o.predict(dpmodel, inputdata.hex)
  return(predictionsS)
}
