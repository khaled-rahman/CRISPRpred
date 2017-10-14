data <- read.csv("FC_plus_RES.csv")
sgrnas <- data$X30mer
sgrnalist <- unlist(sgrnas)
sgrnaLength <- 30
aVal <- c(1:sgrnaLength) * 0
cVal <- c(1:sgrnaLength) * 0
gVal <- c(1:sgrnaLength) * 0
tVal <- c(1:sgrnaLength) * 0

for (i in 1:length(sgrnalist)) {
  s <- toString(sgrnas[i])
  for (j in 1:sgrnaLength) {
    if (j %in% gregexpr(pattern = "A", s)[[1]]) {
      aVal[j] <- aVal[j] + 1
    }
    if (j %in% gregexpr(pattern = "C", s)[[1]]) {
      cVal[j] <- cVal[j] + 1
    }
    if (j %in% gregexpr(pattern = "G", s)[[1]]) {
      gVal[j] <- gVal[j] + 1
    }
    if (j %in% gregexpr(pattern = "T", s)[[1]]) {
      tVal[j] <- tVal[j] + 1
    }
  }
}
aVal <- aVal / length(sgrnalist)
cVal <- cVal / length(sgrnalist)
gVal <- gVal / length(sgrnalist)
tVal <- tVal / length(sgrnalist)
#cat('A:',aVal, '\n','C:', cVal, '\n','G:', gVal, '\n','T:', tVal,'\n')
#sgrnalist <- unlist(sgrnas)
#print(length(sgrnas))

#xAxis = c(
#  "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"
#)
xAxis = c(
  "-4","-3","-2","-1","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","N","G", "G","+1","+2","+3")

data <- matrix(c(aVal,cVal,gVal,tVal),nrow = 4, ncol = 30,byrow = TRUE)
colours <- c("red", "blue", "yellow", "green")
barplot(
  data, xlab = "Position", ylab = "Normalized Read Count", cex.lab = 1.2, cex.axis = 1.2, cex.names = 0.6, beside =
    TRUE, col = colours, names.arg = xAxis, axes = TRUE
)
legend(
  "topleft", c("A","C","G","T"), cex = 1.3, bty =
    "n", fill = colours
)

aps = .8125
api = .78125
agap = .875
com = .965
maps = .28716
mpi = .36242
mgap=.6636
mcom=.91
allscore = c(aps,maps,api,mpi,agap,mgap,com,mcom)
allmat = matrix(allscore, nrow = 2, ncol = 4,byrow = FALSE)
colr = c("red","blue")
xas = c("PSF","PIF","GAP","COM")
barplot(
  allmat, xlab = "Feature Category", ylab = "Accuracy and MCC Value", cex.lab = 1.2, cex.axis = 1.2, cex.names = 1.1, beside =
    TRUE, col = colr, names.arg = xas, axes = TRUE, ylim = c(0,1)
)
legend(
  "topleft", c("Accuracy x 100%", "MCC"), cex = 1.3, bty =
    "n", fill = colr
)