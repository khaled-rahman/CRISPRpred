#' Illustration of calculateclassificationscore
#'
#' This function takes predicted and observed value as input and produce accuracy, auROC and auPR values. In addition it also draws several plots.
#'
#' @param name a name of the classification method
#' @param predictedValue a list of predicted value
#' @param trueValue a list of true observed value
#' @return a dataframe containing accuracy auroc and aupr values
#' @export
#' @examples
#' setwd("..")
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' prediction = data$predictions
#' data$predictions = NULL
#' observed = data$score_drug_gene_threshold
#' features = calculateclassificationscore("Simple Method", prediction, observed)
calculateclassificationscore <- function(name = "Temporary", predictedValue, trueValue){
  pred <- prediction(predictedValue, trueValue)
  roccurve <- performance(pred,"tpr","fpr")
  par(mar = c(4, 5, 1.98, 1))
  plot(roccurve, main = name, cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis=1.3,yaxis.cex.axis=1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1)
  dev.copy(pdf, "ROCcurve.pdf")
  dev.off()
  pr <- performance(pred, "prec", "rec")
  par(mar = c(4, 5, 1.98, 1))
  plot(pr, main = name, cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis=1.3,yaxis.cex.axis=1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1)
  
  dev.copy(pdf, "PRcurve.pdf")
  dev.off()
  ss <- performance(pred, "sens", "spec")
  par(mar = c(4, 5, 1.98, 1))
  plot(ss, main = name, cex.main = 1.7, cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis=1.3,yaxis.cex.axis=1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1)
  dev.copy(pdf, "SensSpeci.pdf")
  dev.off()
  acc <- performance(pred, "acc")
  par(mar = c(4, 5, 1.98, 1))
  plot(acc,main = name, cex.main = 1.7, xlab = "Threshold", cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis=1.3,yaxis.cex.axis=1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1)
  dev.copy(pdf, "Accuracy.pdf")
  dev.off()
  mcc <- performance(pred, "mat")
  par(mar = c(4, 5, 1.98, 1))
  plot(mcc,main = name, cex.main = 1.7,xlab = "Threshold", cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis=1.3,yaxis.cex.axis=1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1)
  dev.copy(pdf, "MCCcurve.pdf")
  dev.off()
  f <- performance(pred, "f")
  par(mar = c(4, 5, 1.98, 1))
  plot(f, main = name, cex.main = 1.7, xlab = "Threshold", cex.lab = 1.7, box.lty = 7, box.lwd = 4,xaxis.cex.axis=1.3,yaxis.cex.axis=1.3, lwd = 4, yaxis.lwd = 4, xaxis.lwd = 4, yaxis.las = 1)
  dev.copy(pdf, "Fmeasure.pdf")
  dev.off()
  xx.df <- prediction(predictedValue, trueValue)
  perf  <- performance(xx.df, "prec", "rec")
  xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
  xy <- subset(xy, !is.nan(xy$precision))
  xy <- rbind(c(0, 0), xy)
  aucpr  <- trapz(xy$recall, xy$precision)
  auc = performance(pred, "auc")
  roc = auc@y.values[[1]]
  acc = acc@y.values[[1]]
  acc = acc[!is.infinite(acc)]
  acc = max(acc)
  classscore = data.frame(acc, roc, aucpr)
  return(classscore)
}