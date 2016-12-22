#' Illustration of Featureselection
#'
#' This function takes dataset and prediction (observed) value as input and produce a list of features based on a wrapper selection algorithm of Boruta package.
#'
#' @param data provided as dataframe
#' @param prediction a list of true observed value
#' @param iterations number of iterations Boruta package will iterate
#' @return a list of selected features
#' @export
#' @examples
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' prediction = data$predictions
#' data$predictions = NULL
#' features = featureselection(data, prediction, iterations = 100)
featureselection = function(data, prediction, iterations = 200) {
  Fselection <- Boruta(x = data, y = prediction, maxRuns = iterations)
  importantFeature = unlist(lapply(Fselection$finalDecision, function(a)
    if (a == 'Confirmed' || a == 'Tentative') {
      a
    }))
  tentativeFeature = unlist(lapply(Fselection$finalDecision, function(a)
    if (a == 'Tentative') {
      a
    }))
  
  if (length(importantFeature) < 1) {
    mylist = list()
    for (i in names(data)) {
      mylist[i] <- 0
    }
    for (i in 1:length(Fselection$ImpHistory[,1])) {
      for (j in names(Fselection$ImpHistory[i,])) {
        #cat(i,":",j,":", as.numeric(Fselection$ImpHistory[i,j]) ,"\n")
        if (j %in% names(data)) {
          mylist[j] <-
            as.numeric(mylist[j]) + as.numeric(Fselection$ImpHistory[i,j])
        }
      }
    }
    for (i in names(data)) {
      mylist[i] <- as.numeric(mylist[i]) / iterations
      if(is.infinite(as.numeric(mylist[i]))){
        mylist[i] <- NULL
      }
    }
    index = sort.list(as.numeric(mylist), decreasing = TRUE, na.last = NA ,method = "quick")
    rankedlist = list()
    for (i in 1:length(index)) {
      rankedlist = c(rankedlist, mylist[index[i]])
    }
    importantFeature = rankedlist
  }
  importantFeature = names(importantFeature)
  return(importantFeature)
}