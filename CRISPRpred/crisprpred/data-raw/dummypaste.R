library(caTools)
library(e1071)
library(ROCR)
mainname = "ROC-Curve"
#svm1 = read.csv('svm_1.csv')
#svm164 = read.csv('svm_0.164.csv')
#svm1758 = read.csv('svm_0.1758.csv')
#svm1925 = read.csv('svm_0.1925.csv')
#svm2 = read.csv('svm_2.csv')
#svm4 = read.csv('svm_40.csv')
#svm5 = read.csv('svm_50.csv')
#svmpred = svm5
#svmpred = read.csv('svm_0.1758.csv')
svmpred$X = NULL
svmpred = as.vector(svmpred$x)
sortedresults = read.csv('sortedontargetresults.csv')
azimuth = sortedresults$azimuth
trueV = sortedresults$result
svmprediction = prediction(svmpred, trueV)
svmROC = performance(svmprediction,"tpr","fpr")
svmACC = performance(svmprediction, "acc")
svmAUC = performance(svmprediction, "auc")
svmPR = performance(svmprediction,"prec","rec")
svmSS = performance(svmprediction,"sens","spec")
svmMCC = performance(svmprediction,"mat")
perf  <- performance(svmprediction, "prec", "rec")

xy    <-
  data.frame(recall = perf@x.values[[1]], precision = perf@y.values[[1]])
xy <- subset(xy,!is.nan(xy$precision))
xy <- rbind(c(0, 0), xy)
svmAUPR  <- trapz(xy$recall, xy$precision)
cat('AUC', svmAUC@y.values[[1]], '\n')
cat('AUPR', svmAUPR, '\n')
#cat('SVM MCC = ', max(svmMCC@y.values),'\n')
par(mar = c(4, 5, 1.98, 1))
#plot(
#  svmROC, main = mainname, cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
#    1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1, col = c('brown'), add =TRUE
#)
# azimuthprediction = prediction(azimuth, trueV)
# azimuthROC = performance(azimuthprediction,"tpr","fpr")
# azimuthPR = performance(azimuthprediction,"prec","rec")
# azimuthAUC = performance(azimuthprediction, "auc")
# azimuthACC = performance(azimuthprediction, "acc")
# azimuthSS = performance(azimuthprediction, "sens","spec")
# perf  <- performance(azimuthprediction, "prec", "rec")
# azimuthMCC  <- performance(azimuthprediction, "mat")
# xy    <-
#   data.frame(recall = perf@x.values[[1]], precision = perf@y.values[[1]])
# xy <- subset(xy,!is.nan(xy$precision))
# xy <- rbind(c(0, 0), xy)
# azimuthAUPR  <- trapz(xy$recall, xy$precision)
# cat('Azimuth AUC = ', azimuthAUC@y.values[[1]], '\n')
# cat('Azimuth AUPR = ', azimuthAUPR, '\n')
# #cat('Azimuth MCC = ', max(azimuthMCC@y.values),'\n')
# plot(
#   azimuthMCC, main = mainname, cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis =
#     1.3,yaxis.cex.axis = 1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1, add = TRUE, col = c('red')
# )
# legend(0.1,0.15, legend=c("0.1", "0.164", "0.1758", "0.1925", "0.4", "0.5"),
#        col=c("black", "blue","green", "red" , "purple", "brown"), lty=1, lwd = 4, cex=1.0, box.lty = 0, horiz = FALSE)

#SVM AUC =  0.847514 
#SVM AUPR =  0.5598399 
#SVM MCC = 0.4732688
#Azimuth AUC =  0.8323232 
#Azimuth AUPR =  0.5348426 
#Azimuth MCC = 0.4320542
