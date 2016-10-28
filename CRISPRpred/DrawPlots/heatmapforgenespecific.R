d <- read.csv("FC_plus_RES.csv")
sgrnas <- d$X30mer
gene <- d$Target.gene
sgrnalist <- unlist(sgrnas)
genelist <- unlist(gene)
mgeneA <- list()
mgeneC <- list()
mgeneG <- list()
mgeneT <- list()
for (i in genelist) {
  mgeneA[i] <- 0
  mgeneC[i] <- 0
  mgeneG[i] <- 0
  mgeneT[i] <- 0
}

sgrnaLength <- 30
# aVal <- c(1:sgrnaLength) * 0
# cVal <- c(1:sgrnaLength) * 0
# gVal <- c(1:sgrnaLength) * 0
# tVal <- c(1:sgrnaLength) * 0

for (i in 1:length(sgrnalist)) {
  s <- toString(sgrnas[i])
  g <- toString(gene[i])
  for (j in 1:sgrnaLength) {
    if (j %in% gregexpr(pattern = "A", s)[[1]]) {
      mgeneA[g] <- as.numeric(mgeneA[g]) + 1
    }
    if (j %in% gregexpr(pattern = "C", s)[[1]]) {
      mgeneC[g] <- as.numeric(mgeneC[g]) + 1
    }
    if (j %in% gregexpr(pattern = "G", s)[[1]]) {
      mgeneG[g] <- as.numeric(mgeneG[g]) + 1
    }
    if (j %in% gregexpr(pattern = "T", s)[[1]]) {
      mgeneT[g] <- as.numeric(mgeneT[g]) + 1
    }
  }
}
cnames <- c("A", "C", "G", "T")
rnames <- names(mgeneA)
p <-
  matrix(
    c(
      as.numeric(mgeneA), as.numeric(mgeneC), as.numeric(mgeneG), as.numeric(mgeneT)
    ), nrow = 17, ncol = 4
  )
rownames(p,do.NULL = TRUE, prefix = "row")
colnames(p,do.NULL = TRUE, prefix = "col")
rownames(p) <- rnames
colnames(p) <- cnames
for (i in 1:17) {
  p[i,] <- p[i,] / sum(p[i,])
}
finalData <- p
# lmat <- rbind(c(2,3),c(4,1))
# lwid = c(1.5,4)
# lhei = c(1.5,4)
heatmap.2(
  finalData, Rowv = NA, Colv = NA, scale = "none", keep.dendro = FALSE, col = redblue(100),trace = "none", density.info = "none", key.title = "Key", key.xlab = "Normalized Value", main = "Gene-wise nucleotides distribution"
)