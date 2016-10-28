#' Deep Learning
#'
#' This function takes deep learning feature list, dataset, leave one out value, a value for n-fold cross-validation and learning rate. Now, it outputs spearman correlation based on provided dataset.
#' @param featurelist a list of feature
#' @param featuredata provided dataset
#' @param leaveonegene 1 means this method will perform leave-one-out cross-validation; otherwise it will perform n-fold cross-validation
#' @param kfold a value for cross validation, by default it is set to 10
#' @param learningrrate learning rate value. Default one is set to 0.6
#' @return a vector of spearman correlations for all runs
#' @export
#' @examples
#' featurelist = c("X30mer", "Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' #suppose we have a file as '../crisprpred/data-raw/sample_data.csv' and current directory is set to '../crisprpred'
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample_data.csv')
#' data = read.csv(filepath)
#' dplearning(featurelist, data,leaveonegene=1)
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
    step = length(featuredata[,1]) / kfold
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
