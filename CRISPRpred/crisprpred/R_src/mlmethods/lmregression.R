#' Linear Regression
#'
#' This function takes featurelist, dataset and a value for cross-validation. Now, it creates a formula and outputs RMSE based on provided dataset.
#' @param featurelist a list of feature
#' @param featuredata provided dataset
#' @param kfold a value for cross validation
#' @return nothing
#' @export
#' @examples
#' featurelist = c("X30mer", "Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' input = featurization(input,featurelist)
#' lmregression(featurelist,input,3)
lmregression = function(featurelist, featuredata,leaveonegene = 0, kfold = 10) {
  fformula = featureformula(featurelist)
  if (leaveonegene == 0) {
    model1 = lm(as.formula(fformula), featuredata)
    model2 = CVlm(data = featuredata, form.lm = model1, m = kfold)
    predictionsS = predict(model2, featuredata)
    j = 1
    rmseS = c()
    spcor = c()
    step = length(data[,1]) / kfold
    for (i in 1:(kfold - 1)) {
      errorS = featuredata$predictions[j:(j + step)] - predictionsS[j:(j + step)]
      rmseS = c(rmseS, rmse(errorS))
      spcor = c(spcor, stats::cor(featuredata$predictions[j:(j + step)], predictionsS[j:(j +
                                                                                    step)], method = 'spearman'))
      j = j + step
    }
    cat("LR Regression RMSE (n-fold):", rmseS, "\n")
    cat("Spearman Cor. for LR (n-fold):", spcor, "\n")
    
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
      model1 = lm(as.formula(fformula), featuredata)
      #model2 = CVlm(data = featuredata, form.lm = model1)
      predictionsS = predict(model1, testing)
      spcor = c(spcor, stats::cor(as.vector(predictionsS), as.vector(testing$predictions), method = 'spearman'))
      
      errorS = predictionsS - testing$predictions
      rmseS = c(rmseS, rmse(errorS))
    }
    cat("LR Regression RMSE (leave one gene out):", rmseS, "\n")
    cat("Spearman Cor. for LR (leave one gene out):", spcor, "\n")
    
  }
  return(spcor)
}
