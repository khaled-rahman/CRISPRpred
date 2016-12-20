pkgname <- "crisprpred"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('crisprpred')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("countpattern")
### * countpattern

flush(stderr()); flush(stdout())

### Name: countpattern
### Title: Illustration of countpattern This function takes sequence and
###   pattern as input and count how many times a particular pattern is
###   present in the sequence.
### Aliases: countpattern

### ** Examples

sequence = list("ABDEFGHABDAACBBDEBGGGHHH", "ABCBDBEBEBBBDBDBFDFDFGGHHEEFFEECCCD")
pattern = "BD"
feat = featurization(sequence, pattern)
feat



cleanEx()
nameEx("crisprpred_main")
### * crisprpred_main

flush(stderr()); flush(stdout())

### Name: crisprpred_main
### Title: Explanation of crisprpred_main functions
### Aliases: crisprpred_main

### ** Examples

setwd('..')
#suppose we have a file as '../crisprpred/data-raw/sample.csv' and current directory is set to '../crisprpred'
dir = getwd()
datasetpath = paste0(dir,'/data-raw/sample.csv')
featurelist = c("X30mer", "Percent.Peptide", "Amino.Acid.Cut.position","predictions")
kfoldCross = 2
crisprpred_main(datasetpath, featurelist, kfoldCross, 3, 4, 0.66)



cleanEx()
nameEx("dplearning")
### * dplearning

flush(stderr()); flush(stdout())

### Name: dplearning
### Title: Deep Learning
### Aliases: dplearning

### ** Examples

featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#suppose we have a file as '../crisprpred/data-raw/sample.csv' and current directory is set to '../crisprpred'
dir = getwd()
filepath = paste0(dir,'/data-raw/sample.csv')
data = read.csv(filepath)
dplearning(featurelist,data)



cleanEx()
nameEx("featureformula")
### * featureformula

flush(stderr()); flush(stdout())

### Name: featureformula
### Title: Making Formula for Learning
### Aliases: featureformula

### ** Examples

featurelist = c('X1', 'X2', 'X3', 'Y')
formula = featureformula(featurelist)
formula



cleanEx()
nameEx("featurization")
### * featurization

flush(stderr()); flush(stdout())

### Name: featurization
### Title: Illustration of Featurization
### Aliases: featurization

### ** Examples

input = list("ABCDEFGHABDAACBBDEBGGGHHH", "ABCBDBEBEBBBDBDBFDFDFGGHHEEFFEECCCD")
string = c("A", "BD")
featuredata = featurization(input, string, seq = TRUE, pos = FALSE)
featuredata



cleanEx()
nameEx("findposition")
### * findposition

flush(stderr()); flush(stdout())

### Name: findposition
### Title: Illustration of findposition This function takes sequence,
###   pattern and position as input and check whether a particulaer pattern
###   is present in position-th place of sequence.
### Aliases: findposition

### ** Examples

sequence = list("ABDEFGHABDAACBBDEBGGGHHH", "ABCBDBEBEBBBDBDBFDFDFGGHHEEFFEECCCD")
pattern = "BD"
position = 2
feat = featurization(sequence, pattern, position)
feat



cleanEx()
nameEx("lmregression")
### * lmregression

flush(stderr()); flush(stdout())

### Name: lmregression
### Title: Linear Regression
### Aliases: lmregression

### ** Examples

featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
dir = getwd()
filepath = paste0(dir,'/data-raw/sample.csv')
data = read.csv(filepath)
lmregression(featurelist,data,0)



cleanEx()
nameEx("mars")
### * mars

flush(stderr()); flush(stdout())

### Name: mars
### Title: MARS Regression
### Aliases: mars

### ** Examples

featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
dir = getwd()
filepath = paste0(dir,'/data-raw/sample.csv')
data = read.csv(filepath)
mars(featurelist,data,0,2)



cleanEx()
nameEx("randomforest")
### * randomforest

flush(stderr()); flush(stdout())

### Name: randomforest
### Title: Random Forest
### Aliases: randomforest

### ** Examples

featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#suppose we have a file as '../crisprpred/data-raw/sample.csv' and current directory is set to '../crisprpred'
dir = getwd()
filepath = paste0(dir,'/data-raw/sample.csv')
data = read.csv(filepath)
randomforest(featurelist,data)



cleanEx()
nameEx("randomforest0")
### * randomforest0

flush(stderr()); flush(stdout())

### Name: randomforest0
### Title: Random Forest
### Aliases: randomforest0

### ** Examples

featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
#suppose we have a file as '../crisprpred/data-raw/sample.csv' and current directory is set to '../crisprpred'
dir = getwd()
filepath = paste0(dir,'/data-raw/sample.csv')
data = read.csv(filepath)
randomforest(featurelist,data,leaveonegene = 0)



cleanEx()
nameEx("rmse")
### * rmse

flush(stderr()); flush(stdout())

### Name: rmse
### Title: Root Mean Square Error
### Aliases: rmse

### ** Examples

rmse(5)



cleanEx()
nameEx("svmregression")
### * svmregression

flush(stderr()); flush(stdout())

### Name: svmregression
### Title: SMV Regression
### Aliases: svmregression

### ** Examples

featurelist = c("Percent.Peptide", "Amino.Acid.Cut.position","predictions")
dir = getwd()
filepath = paste0(dir,'/data-raw/sample.csv')
data = read.csv(filepath)
svmregression(featurelist,data,0)



cleanEx()
nameEx("viennaRNADataManipulation")
### * viennaRNADataManipulation

flush(stderr()); flush(stdout())

### Name: viennaRNADataManipulation
### Title: Description of viennaRNADataManipulation Function
### Aliases: viennaRNADataManipulation

### ** Examples

s = c('AGGCGTGTTAACT','ACGTTTAAGCT')
viennaRNADataManipulation(s)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
