#' Illustration of Data Partition
#' 
#' This function takes data, target column name, boolean value of leaveonegeneout, gene name and a value for nfold as input and produce a list of trining and testing datasets.
#'
#' @param data a dataframe object to be partitioned into training and testing set
#' @param targetcolumn a string indicating which column contains gene information
#' @param leaveonegene true means partition data based on leave one out; otherwise based on nfold
#' @param genename a string indicating name of gene to be out
#' @param nfold a value indicating data to be partitioned based on nfold cross-validation
#' @return a list containing training and testing datasets
#' @export
#' @examples
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample.csv')
#' data = read.csv(filepath)
#' dataset = datapartition(data, "Target.gene", leaveonegene = TRUE, genename = "CD5")
#' dataset = datapartition(data, nfold = 5)
datapartition = function(data, targetcolumn, leaveonegene = FALSE, genename, nfold = 10) {
  if (leaveonegene == TRUE) {
    if (!genename %in% data[targetcolumn][,1]) {
      cat("Please provide a valid Gene name\n")
    }else{
      training = data[!data[targetcolumn][,1] %in% genename,]
      testing = data[data[targetcolumn][,1] %in% genename,]
    }
  }else{
    len = length(data[,1]) / nfold
    training = 1:(len * (nfold - 1))
    testing = (length(training) + 1):length(data[,1])
    training = data[training,]
    testing = data[testing,]
  }
  return(list(training,testing))
}