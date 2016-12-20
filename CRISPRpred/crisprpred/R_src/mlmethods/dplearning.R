#' Deep Learning
#'
#' This function takes full filepath, a list of learning features, a value for cross-validation, the number of times data set will be iterated and learning rate. Now, it creates a deep learning model using  deeplearning function of h2o package and outputs RMSE based on provided dataset. Note that size of dataset should be enough to choose a suitable value for kfold.
#' @param filepath a full path of the csv file
#' @param featurelist a list of features. last name will indicate the value to be predicted.
#' @param kfold a value for cross validation. Default value is 3.
#' @param iterations number of times dataset will be iterated. Default value is 1.
#' @param learningrate a fractional learning rate for deep learner. Default value is 0.6.
#' @return nothing
#' @export
#' @examples
#' featurelist = c("X30mer", "Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' #suppose we have a file as '../crisprpred/data-raw/sample_data.csv' and current directory is set to '../crisprpred'
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample_data.csv')
#' dplearning(filepath,featurelist,3,1,0.56)
dplearning = function(featurelist,featuredata,leaveonegene = 0, kfold = 10, learningrate = 0.6) {
  predict = featurelist[length(featurelist)]
  featurelist = featurelist[-length(featurelist)]
  if (leaveonegene == 0) {
    preddata = as.vector(data[predict][,])
    h2o.init()
    inputdata.hex = as.h2o(featuredata, destination_frame = "inputdata.hex")
    dpmodel = h2o.deeplearning(
      x = featurelist, y = predict, training_frame = inputdata.hex, nfolds = kfold, rate = learningrate
    )
    dppred = h2o.predict(dpmodel, inputdata.hex)
    #cat('DP:',as.vector(dppred),'\n')
    j = 1
    rmseE = c()
    spcor = c()
    step = length(data[,1]) / kfold
    for (i in 1:(kfold - 1)) {
      rmseE = c(rmseE,rmse(preddata[j:(j + step)] - as.vector(dppred)[j:(j + step)]))
      spcor = c(spcor,cor(preddata[j:(j + step)], as.vector(dppred)[j:(j + step)], method = 'spearman'))
      j = j + step;
    }
    cat("Deep Learning RMSE (n-fold):", rmseE,"\n")
    cat("Spearman Cor. for DL (n-fold):", spcor, "\n")
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
      modelS = h2o.deeplearning(
        x = featurelist, y = predict, training_frame = training.hex,rate = learningrate
      )
      predictionsS = h2o.predict(modelS, testing.hex)
      spcor = c(spcor, stats::cor(
        as.vector(predictionsS), as.vector(testing$predictions), method = 'spearman'
      ))
      errorS = as.vector(predictionsS) - as.vector(testing$predictions)
      rmseS = c(rmseS, rmse(errorS))
    }
    cat("DL RMSE (leave one gene out):", rmseS, "\n")
    cat("Spearman Cor. for DL (leave one gene out):", spcor, "\n")
    
  }
  return(spcor)
}
