library(Boruta)
iterations = 200
data <- read.csv('FC_plus_RES_withPredictions.csv')
data$X <- NULL
data$X30mer <- NULL
data$Target.gene <- NULL
data$drug <- NULL
data$Heat <- NULL
y <- data$predictions
data$predictions <- NULL
Fselection <- Boruta(x = data, y = y, maxRuns = iterations)
importantFeature = unlist(lapply(Fselection$finalDecision, function(a)
  if (a == 'Confirmed' || a == 'Tentative') {
    a
  }))
tentativeFeature = unlist(lapply(Fselection$finalDecision, function(a)
  if (a == 'Tentative') {
    a
  }))
for (i in names(data)) {
  if (!i %in% names(importantFeature)) {
    data[i] <- NULL
  }
}

btrain <- Boruta(x = data, y = y)

plot(btrain, xlab = "", xaxt = "n")
lz <- lapply(1:ncol(btrain$ImpHistory),function(i)
  btrain$ImpHistory[is.finite(btrain$ImpHistory[,i]),i])
names(lz) <- colnames(btrain$ImpHistory)
Labels <- sort(sapply(lz,median))

axis(
  side = 1,las = 2,labels = names(Labels),
  at = 1:ncol(btrain$ImpHistory), cex.axis = 0.7
)

dev.copy(pdf, 'varianceFeatures.pdf')
dev.off()

mylist = list()
for (i in names(data)) {
  mylist[i] <- 0
}
for (i in 1:iterations) {
  for (j in names(Fselection$ImpHistory[i,])) {
    if (j %in% names(importantFeature)) {
      mylist[j] <-
        as.numeric(mylist[j]) + as.numeric(Fselection$ImpHistory[i,j])
    }
  }
}
for (i in names(data)) {
  mylist[i] <- as.numeric(mylist[i]) / iterations
}

index = sort.list(as.numeric(mylist), decreasing = TRUE, method = "radix")

rankedlist = list()

#selecting top 50 features based on Z-Scores#

for (i in 1:50) {
  rankedlist = c(rankedlist, mylist[index[i]])
}

for (i in names(data)) {
  if (!i %in% names(rankedlist)) {
    data[i] <- NULL
  }
}
write.csv(data, 'FinalFeaturedData.csv')

#data <- read.csv('FC_plus_RES_withPredictions.csv')
#data <- read.csv('FinalFeaturedData.csv')
trainingdata = data[1:4050,]
testingdata = data[4051:4500,]
features = names(rankedlist)
features[length(features) + 1] = c('predictions')
formuli = featureformula(features)
model = lm(as.formula(formuli), trainingdata)
predictions = predict(model, testingdata)
trueValue = testingdata$predictions
err = trueValue - predictions
Error = rmse(err)
SSC = cor(trueValue, predictions, method = 'spearman')

#anova test#
ap = anova(model)[5]
p = ap$`Pr(>F)`
f = row.names(ap)
p = p[-length(p)]
f = f[-length(f)]
for (i in 1:length(p)) {
  if (p[i] > 0.0001) {
    p = p[-i]
    f = f[-i]
  }
}
featurelist = f
for (i in 1:length(rankedlist)) {
  if (!names(rankedlist[i]) %in% featurelist)
  {
    rankedlist = rankedlist[-i]
  }
}

#selecting top30 features after anova test#


flist = list()

for(i in 1:30)
{
  d = c()
  for(j in 1:19)
  {
    #print(names(rankedlist[i]))
    if(names(rankedlist[i]) %in% names(Fselection$ImpHistory[j,]))
    {
      d = c(d, Fselection$ImpHistory[j,][names(rankedlist[i])])
    }
  }
  flist[i] = list(d)
}
par(mar = c(7, 4, 2, 2) + 0.2) #add room for the rotated labels
end_point = 0.5 + length(flist) + length(flist) - 1
barplot(
  as.numeric(flist), col = "blue",
  main = "",
  ylab = "Z-Scores", ylim = c(0,5 + max(as.numeric(rankedlist))),
  xlab = "Features",
  space = 1
)
text(
  seq(1.5,end_point,by = 2), par("usr")[3] - 0.25,
  srt = 60, adj = 1, xpd = TRUE,
  labels = paste(names(flist)), cex = 0.65
)
dev.copy(pdf, 'ImportantFeatures.pdf')
dev.off()
