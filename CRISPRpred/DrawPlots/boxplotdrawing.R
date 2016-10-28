library(h2o)
##########boxplot coding###############
mlist = list()
for (i in names(rankedlist)) {
  mlist[[i]] <- 0
}
for (i in 1:19) {
  for (j in names(Fselection$ImpHistory[i,])) {
    if (j %in% names(rankedlist)) {
      mlist[[j]] <- c(mlist[[j]],as.numeric(Fselection$ImpHistory[i,j]))
    }
  }
}
for (i in 1:30) {
  mlist[[i]][1] = NA
}
t = lapply(mlist, function (x)
  x[!is.na(x)])
par(mar = c(9.5, 4, 1, 1) + 0.2) #add room for the rotated labels
end_point = 0.5 + length(t) + length(t) - 1
boxplot(
  t, col = "grey50",
  main = "",
  ylab = "Z-Scores", ylim = c(min(as.numeric(t[[30]])) - 1,1 + max(as.numeric(t[[1]]))),
  xlab = "Features",
  space = 1, names = NA
)
text(
  seq(1.2,end_point / 2 + 1,by = 1), par("usr")[3] - 0.25,
  srt = 62, adj = 1, xpd = TRUE,
  labels = paste(names(t)), cex = 0.65
)
#boxplot(ld, col = c('green','grey','blue','red','orange'), xlab = "Machine Learning Algorithms",ylab="Spearman Correlation")
# boxplot(rld, col = c('green','grey','blue','red','orange'), xlab = "Machine Learning Algorithms",ylab="RMSE")

#save results in rrfd for different number of allowed trees#
rrfd = list()
for (i in 1:30) {
  r = mean(randomforest(
    filepath = path, featurelist = featurelist, trees = as.integer(i)
  ))
  rrfd[i] = r
}

#save results for different values of sample rate in rrsr#
rrsr = list()
for (i in 1:99) {
  r = mean(randomforest(
    filepath = path, featurelist = featurelist, trees = 30, samplerate = as.double(i/100)
  ))
  rrsr[i] = r
}