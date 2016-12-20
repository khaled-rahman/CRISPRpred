#' Random Forest
#'
#' This function takes full filepath, a list of learning features, a value for cross-validation, the number of times data set will be iterated and learning rate. Now, it creates a deep learning model using  deeplearning function of h2o package and outputs RMSE based on provided dataset. Note that size of dataset should be enough to choose a suitable value for kfold.
#' @param featurelist a list of features. last name will indicate the value to be predicted.
#' @param featuredata a sample dataset containing all features
#' @param leaveonegene check for leaveonegeneout cross-validation
#' @param kfold a value for cross validation. Default value is 3.
#' @param trees number of trees that will be built.
#' @return spearman correlation
#' @export
#' @examples
#' featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' #suppose we have a file as '../crisprpred/data-raw/sample.csv' and current directory is set to '../crisprpred'
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' randomforest(featurelist,data,leaveonegene = 0)

randomforest0 = function(featurelist, featuredata,leaveonegene = 0, kfold = 10, trees = 500) {
  fformula = featureformula(featurelist)
  if (leaveonegene == 0) {
    modelS = randomForest(as.formula(fformula), featuredata, proximity = FALSE, importance = TRUE, ntree = trees)
    predictionsS = predict(modelS, featuredata)
    j = 1
    rmseS = c()
    spcor = c()
    step = length(data[,1]) / kfold
    for (i in 1:(kfold - 1)) {
      errorS = featuredata$predictions[j:(j + step)] - predictionsS[j:(j + step)]
      rmseS = c(rmseS, rmse(errorS))
      spcor = c(spcor, cor(featuredata$predictions[j:(j + step)], predictionsS[j:(j +
                                                                                    step)], method = 'spearman'))
      j = j + step
    }
    cat("RF Regression RMSE (n-fold):", rmseS, "\n")
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
      modelS = randomForest(as.formula(fformula), training, proximity = FALSE, importance = TRUE, ntree = trees)
      predictionsS = predict(modelS, testing)
      spcor = c(spcor, stats::cor(as.vector(predictionsS), as.vector(testing$predictions), method = 'spearman'))
      
      errorS = predictionsS - testing$predictions
      rmseS = c(rmseS, rmse(errorS))
    }
    cat("RF Regression RMSE (leave one gene out):", rmseS, "\n")
    cat("Spearman Cor. for RF (leave one gene out):", spcor, "\n")
    
  }
  return(spcor)
}
