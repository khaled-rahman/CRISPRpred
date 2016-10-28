#' Explanation of crisprpred_main functions
#'
#' This function takes full datasetpath and reads data as a R object (data frame), a list of features and a number to denote cross-validation. It also takes other parameters for different algorithms. Then, it performs Machine Learning algorithms and build prediction models. Then it predicts sgRNA activity based on prediction models.
#' @param datasetpath full path of a csv file
#' @param featurelist provided by user as a list of strings
#' @param kfold used in ML-functions for kfold cross-validation 
#' @param iteration4dl number of time dataset will be iterated
#' @param trees number of trees in random forest
#' @param learningrate learning rate of deep learner
#' @param samplingrate sampling rate in random forest
#' @return None
#' @export
#' @examples 
#' setwd('..')
#' #suppose we have a file as '../crisprpred/data-raw/sample_data.csv' and current directory is set to '../crisprpred'
#' dir = getwd()
#' datasetpath = paste0(dir,'/data-raw/sample_data.csv')
#' featurelist = c("X30mer", "Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#' kfoldCross = 2
#' crisprpred_main(datasetpath, featurelist, kfoldCross, 3, 4, 0.66)

crisprpred_main = function(datasetpath, featurelist, kfold, iteration4dl, trees, learningrate, samplingrate){
  #dataset = read.csv(datasetpath)
  #featuredata = featurization(dataset, featurelist)
  #write.csv(featuredata, "features-data.csv")
  #loadData(datasetpath, featurelist)
  featuredata = read.csv(datasetpath)
  #featuredata = featurization(dataset, featurelist)
  #write.csv(featuredata, datasetpath, row.names = FALSE)

  lmregression(featurelist, featuredata, kfold)
  
  svmregression(featurelist, featuredata, kfold)

  dplearning(datasetpath, featurelist, kfold, iteration4dl, learningrate)

  randomforest(datasetpath, featurelist, kfold, trees, samplingrate)
}
