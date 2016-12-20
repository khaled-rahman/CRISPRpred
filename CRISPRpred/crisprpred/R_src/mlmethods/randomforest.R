#' Random Forest
#'
#' This function takes full filepath, a list of learning features, a value for cross-validation, the number of times data set will be iterated and learning rate. Now, it creates a deep learning model using  deeplearning function of h2o package and outputs RMSE based on provided dataset. Note that size of dataset should be enough to choose a suitable value for kfold.
#' @param featurelist a list of features. last name will indicate the value to be predicted.
#' @param featuredata a sample dataset containing all features
#' @param leaveonegene check for leaveonegeneout cross-validation
#' @param kfold a value for cross validation. Default value is 3.
#' @param trees number of trees that will be built.
#' @param learningrate a fractional sampling rate in random forest. Default value is 0.6.
#' @return spearman correlation
#' @export
#' @examples
#' featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' #suppose we have a file as '../crisprpred/data-raw/sample.csv' and current directory is set to '../crisprpred'
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' randomforest(featurelist,data)
randomforest = function(featurelist, featuredata,leaveonegene = 0, kfold = 10, trees = 50, learningrate = 0.6) {
  predict = featurelist[length(featurelist)]
  featurelist = featurelist[-length(featurelist)]
  if (leaveonegene == 0) {
    preddata = as.vector(data[predict][,])
    h2o.init()
    inputdata.hex = as.h2o(featuredata, destination_frame = "inputdata.hex")
    randmodel = h2o.randomForest(
      x = featurelist, y = predict, training_frame = inputdata.hex, nfolds = kfold, ntrees = trees, sample_rate = learningrate
    )
    rfpred = h2o.predict(randmodel, inputdata.hex)
    #cat('DP:',as.vector(dppred),'\n')
    j = 1
    rmseE = c()
    spcor = c()
    step = length(data[,1]) / kfold
    for (i in 1:(kfold - 1)) {
      rmseE = c(rmseE,rmse(preddata[j:(j + step)] - as.vector(rfpred)[j:(j + step)]))
      spcor = c(spcor,cor(preddata[j:(j + step)], as.vector(rfpred)[j:(j + step)], method = 'spearman'))
      j = j + step;
    }
    cat("Random Forest RMSE (n-fold):", rmseE,"\n")
    cat("Spearman Cor. for RF (n-fold):", spcor, "\n")
  }else
  {
    nameofgene = c(
      "CD5", "NF1", "CUL3", "MED12", "TADA2B", "TADA1" , "CD45", "HPRT1", "THY1", "H2-K", "CD28", "NF2", "CD43", "CD33", "CD13", "CCDC101", "CD15"
    )
    rmseS = c()
    spcor = c()
    for (i in nameofgene)
    {
      training = featuredata[!featuredata$Target.gene %in% i,]
      testing = featuredata[featuredata$Target.gene %in% i,]
      training.hex = as.h2o(training, destination_frame = "training.hex")
      testing.hex = as.h2o(testing, destination_frame = "testing.hex")
      modelS = h2o.randomForest(
        x = featurelist, y = predict, training_frame = training.hex,ntrees = trees, sample_rate = learningrate
      )
      predictionsS = h2o.predict(modelS, testing.hex)
      spcor = c(spcor, stats::cor(
        as.vector(predictionsS), as.vector(testing$predictions), method = 'spearman'
      ))
      errorS = as.vector(predictionsS) - as.vector(testing$predictions)
      rmseS = c(rmseS, rmse(errorS))
    }
    cat("Random Forest RMSE (leave one gene out):", rmseS, "\n")
    cat("Spearman Cor. for RF (leave one gene out):", spcor, "\n")
    
  }
  return(spcor)
}
