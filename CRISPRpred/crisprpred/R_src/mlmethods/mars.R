#' MARS Regression
#'
#' This function takes mars regression formula, dataset and a value for cross-validation. Now, it outputs RMSE based on provided dataset.
#' @param featurelist a list of features
#' @param featuredata provided dataset
#' @param kfold a value for cross validation
#' @return nothing
#' @export 
#' @examples
#' featurelist = c("X30mer", "Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' input = featurization(input,featurelist)
#' mars(featurelist,input,3)
mars = function(featurelist, featuredata,leaveonegene = 0, kfold = 10){
  fformula =featureformula(featurelist)
  if(leaveonegene == 0){
  modelE = earth(as.formula(fformula), featuredata, nfold = kfold)
  predictionsE = predict(modelE, featuredata)
  j = 1
  rmseE = c()
  spcor = c()
  step = length(data[,1])/kfold
  for (i in 1:(kfold-1)) {
  errorE = featuredata$predictions[j:(j+step)] - predictionsE[j:(j+step)]
  rmseE = c(rmseE, rmse(errorE))
  spcor = c(spcor,cor(featuredata$predictions[j:(j+step)], predictionsE[j:(j+step)], method='spearman'))
  j = j + step
  }
  cat("MARS Regression RMSE (n-fold):", rmseE, "\n")
  cat("Spearman Cor. for MARS (n-fold):", spcor, "\n")
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
      modelS = earth(as.formula(fformula), training)
      predictionsS = predict(modelS, testing)
      spcor = c(spcor, stats::cor(as.vector(predictionsS), as.vector(testing$predictions), method = 'spearman'))
      
      errorS = as.vector(predictionsS) - as.vector(testing$predictions)
      rmseS = c(rmseS, rmse(errorS))
    }
    cat("MARS Regression RMSE (leave one gene out):", rmseS, "\n")
    cat("Spearman Cor. for MARS (leave one gene out):", spcor, "\n")
    
  }
  return(spcor)
}
