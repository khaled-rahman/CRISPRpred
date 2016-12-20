#' SMV Regression
#'
#' This function takes svm regression formula, dataset and a value for cross-validation. Now, it outputs RMSE based on provided dataset.
#' @param featurelist a list of features
#' @param featuredata provided dataset
#' @param kfold a value for cross validation
#' @return nothing
#' @export
#' @examples
#' featurelist = c("X30mer", "Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' input = featurization(input,featurelist)
#' svmregression(featurelist,input,3)
svmregression = function(featurelist,featuredata,leaveonegene = 0, kfold = 10) {
  fformula = featureformula(featurelist)
  if (leaveonegene == 0) {
    modelS = svm(as.formula(fformula), featuredata, cross = kfold)
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
    cat("SVM Regression RMSE (n-fold):", rmseS, "\n")
    cat("Spearman Cor. for SVM (n-fold):", spcor, "\n")
    
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
      modelS = svm(as.formula(fformula), training)
      predictionsS = predict(modelS, testing)
      spcor = c(spcor, stats::cor(as.vector(predictionsS), as.vector(testing$predictions), method = 'spearman'))
      
      errorS = predictionsS - testing$predictions
      rmseS = c(rmseS, rmse(errorS))
    }
    cat("SVM Regression RMSE (leave one gene out):", rmseS, "\n")
    cat("Spearman Cor. for SVM (leave one gene out):", spcor, "\n")
    
  }
  return(spcor)
}
