#' Random Forest
#'
#' This function takes random forest feature list, dataset, leave one out value, a value for n-fold cross-validation and number of trees. Now, it outputs spearman correlation based on provided dataset.
#' @param featurelist a list of feature
#' @param featuredata provided dataset
#' @param leaveonegene 1 means this method will perform leave-one-out cross-validation; otherwise it will perform n-fold cross-validation
#' @param kfold a value for cross validation, by default it is set to 10
#' @param trees number of trees that are allowed to grow. default one is set to 500.
#' @return a vector of spearman correlations for all runs
#' @export
#' @examples
#' featurelist = c("X30mer", "Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' #suppose we have a file as '../crisprpred/data-raw/sample_data.csv' and current directory is set to '../crisprpred'
#' #setwd('..')
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample_data.csv')
#' data = read.csv(filepath)
#' h2o.init()
#' randomforest(featurelist,data,leaveonegene=1)

randomforest0 = function(featurelist, featuredata,leaveonegene = 0, kfold = 10, trees = 500) {
  fformula = featureformula(featurelist)
  if (leaveonegene == 0) {
    modelS = randomForest(as.formula(fformula), featuredata, proximity = FALSE, importance = TRUE, ntree = trees)
    predictionsS = predict(modelS, featuredata)
    j = 1
    rmseS = c()
    spcor = c()
    step = length(featuredata[,1]) / kfold
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
