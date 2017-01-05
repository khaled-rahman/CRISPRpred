library(caTools)
library(e1071)
library(ROCR)
setwd('..')
dir = getwd()
print(dir)

source(paste0(dir,"/R_src/datapartition/datapartition.R"))
source(paste0(dir,"/R_src/featureformula/featureformula.R"))
setwd(paste0(dir,"/data-raw/"))
randomforestfeatures = read.csv('randomForestFeatures.csv')
fcres = read.csv('FC_plus_RES.csv')
ontargetdata = read.csv('onTargetFinalData.csv')
sortedresults = read.csv('sortedontargetresults.csv')
thr = c(1.9, 0.8, 0.7, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05)
for (i in 1:length(thr)) {
  t = thr[i]
  features = c()
  for (i in 1:length(randomforestfeatures[,2])) {
    if (randomforestfeatures[i,2] > t) {
      features = c(features,toString(randomforestfeatures[i,1]))
    }
  }
  features[length(features) + 1] = "geneThreshold"
  cat('Reading features complete: total',  length(features), ' features ...\n')
  
  columns = names(ontargetdata)
  for (i in 1:length(columns)) {
    if (!columns[i] %in% features) {
      ontargetdata[columns[i]] = NULL
    }
  }
  gene = fcres$Target.gene
  ontargetdata = data.frame(ontargetdata, gene)
  cat('Reading featured data complete ...\n')
  
  genes = c(
    'CCDC101', 'CD13','CD15', 'CD28' ,'CD33' ,'CD43' ,'CD45' ,'CD5' ,'CUL3', 'H2-K', 'HPRT1' ,'MED12', 'NF1' ,'NF2', 'TADA1', 'TADA2B', 'THY1'
  )
  data = ontargetdata
  predValue = c()
  for (i in 1:length(genes)) {
    dataset = datapartition(
      data, targetcolumn = "gene", leaveonegene = TRUE, genename = genes[i]
    )
    training = dataset[[1]]
    testing = dataset[[2]]
    bformula = featureformula(features)
    svmmodel = svm(as.formula(bformula), training)
    svmpred = predict(svmmodel, testing)
    svmpred = as.vector(svmpred)
    predValue = c(predValue, svmpred)
    cat("Running SVM iteration no. = ",i,"  ...\n")
  }
  svmpred = predValue
  write.csv(svmpred, paste0('svm_',toString(t),'.csv'))
  
  azimuth = sortedresults$azimuth
  trueV = sortedresults$result
  svmprediction = prediction(svmpred, trueV)
  svmROC = performance(svmprediction,"tpr","fpr")
  svmACC = performance(svmprediction, "acc")
  svmAUC = performance(svmprediction, "auc")
  svmPR = performance(svmprediction,"prec","rec")
  perf  <- performance(svmprediction, "prec", "rec")
  xy    <-
    data.frame(recall = perf@x.values[[1]], precision = perf@y.values[[1]])
  xy <- subset(xy,!is.nan(xy$precision))
  xy <- rbind(c(0, 0), xy)
  svmAUPR  <- trapz(xy$recall, xy$precision)
  cat('SVM AUC = ', svmAUC@y.values[[1]], '\n')
  cat('SVM AUPR = ', svmAUPR, '\n')
  par(mar = c(4, 5, 1.98, 1))
  plot(
    svmROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
      1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1
  )
  
  azimuthprediction = prediction(azimuth, trueV)
  azimuthROC = performance(azimuthprediction,"tpr","fpr")
  plot(
    azimuthROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
      1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1, add = TRUE, col = c('red')
  )
  dev.copy(pdf, paste0("ROCcurve",toString(t),".pdf"))
  dev.off()
  break
}