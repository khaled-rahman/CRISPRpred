library(e1071)
library(DAAG)
library(h2o)
library(earth)
source('../svmregression.R')
source('../lmregression.R')
source('../dplearning.R')
source('../randomforest.R')
source('../mars.R')
setwd('..')
setwd('..')
getpath = getwd()
#print(getpath)
mainpath = paste0(getpath,'/crisprpred_main/crisprpred_main.R')
source(mainpath)
featurepath = paste0(getpath,'/featurization/featurization.R')
source(featurepath)
metricpath = paste0(getpath,'/metrics/rmse.R')
source(metricpath)
formulapath = paste0(getpath, '/featureformula/featureformula.R')
source(formulapath)
setwd('..')
filepath = getwd()
inputFile = paste0(filepath,'/data-raw/sample_data_featurized.csv')
#inputFile = paste0(filepath,'/data-raw/sample_data.csv')
#print(inputFile)
featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position", "G_20", "predictions")
#featurelist = c("Percent Peptide", "Amino Acid Cut position", "G_20", "GC_Count","AA_18", "TT_17", "A", "GG", "MFE", "predictions")
input = read.csv(inputFile)
#lmregression(featurelist, input, 3)
mars(featurelist, input, 3)
#svmregression(featurelist, input, 5)
#dplearning(inputFile, featurelist, 2, 1, 0.55)
#randomforest(inputFile, featurelist, 2, 1, 0.66)
