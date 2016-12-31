columns = names(ontargetdata)
for(i in 1:length(columns)){
  if(!columns[i] %in% features){
    ontargetdata[columns[i]] = NULL
  }
}
genes = c(
  'CCDC101', 'CD13','CD15', 'CD28' ,'CD33' ,'CD43' ,'CD45' ,'CD5' ,'CUL3', 'H2-K', 'HPRT1' ,'MED12', 'NF1' ,'NF2', 'TADA1', 'TADA2B', 'THY1'
)
predValue = c()
for (i in 1:length(genes)) {
  dataset = datapartition(
    data, targetcolumn = "gene", leaveonegene = TRUE, genename = genes[i]
  )
  training = dataset[[1]]
  testing = dataset[[2]]
  bformula = featureformula(borutaF)
  svmmodel = svm(
    as.formula(bformula), training
  )
  svmpred = predict(svmmodel, testing)
  svmpred = as.vector(svmpred)
  predValue = c(predValue, svmpred)
}
cd13 = testing$result
svmprediction = prediction(svmpred, cd13)
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
par(mar = c(4, 5, 1.98, 1))
plot(
  smvROC, main = "ROC-Curve", cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
    1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1
)
