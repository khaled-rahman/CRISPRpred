#' Illustration of Featurization
#'
#' This function takes dataset and a list of features as input and produce a features-wise dataset. The number of columns in returned dataset is equal to the number of features in featurelist.
#'
#' @param sequences provided as dataframe
#' @param string a list of aminoacids or nucleotides
#' @param seq sequence based features. by default it is true.
#' @param seqorder highest number of sequence which will be considered together
#' @param pos position specific features. by default it is true.
#' @param posorder highest number of sequence which will be considered together
#' @return a featurized dataframe
#' @export
#' @examples
#' input = list("ABCDEFGHABDAACBBDEBGGGHHH", "ABCBDBEBEBBBDBDBFDFDFGGHHEEFFEECCCD")
#' string = c("A", "BD")
#' featuredata = featurization(input, string, seq = TRUE, pos = FALSE)
#' featuredata
featurization <-
  function(sequences, string, seq = TRUE, seqorder = 2, pos = TRUE, posorder = 2) {
    total = 0;
    features = data.frame(1:length(sequences))
    colnames(features)[length(features)] = "Serial"
    if (seq == TRUE) {
      for (s in 1:seqorder) {
        permu = gtools::permutations(
          n = length(string), r = s, v = string, repeats.allowed = TRUE
        )
        for (i in 1:length(permu[,1])) {
          temp = countpattern(sequence = sequences, pattern = paste(permu[i,], collapse = ''))
          #cat(length(temp),permu[i,],"\n")
          if (sum(temp) > 0) {
            features = data.frame(features, temp)
            colnames(features)[length(features)] = paste(permu[i,], collapse = '')
          }
          total = total + 1
        }
        cat(s, " order seq. features:", length(permu[,1]), ":total features = ", total, "\n")
      }
    }
    if (pos == TRUE)
    {
      minlength = min(unlist(lapply(sequences, function(s) {
        nchar(toString(s))
      })))
      for (p in 1:posorder) {
        permu = gtools::permutations(
          n = length(string), r = p, v = string, repeats.allowed = TRUE
        )
        ps = 0
        for (i in 1:(length(permu[,1]))) {
          for (j in 1:(minlength - length(permu[i,]) + 1)) {
            #cat("Checking:",paste(permu[i,],collapse = ''),"\n")
            temp = findposition(sequence = sequences, pattern = paste(permu[i,], collapse = ''), j)
            #cat(length(temp),permu[i,],"\n")
            if (sum(temp) > 0) {
              features = data.frame(features, temp)
              colnames(features)[length(features)] = paste0(paste(permu[i,], collapse = ''), "_", j)
            }
            ps = ps + 1
            total = total + 1
          }
        }
        cat(p, "order pos. features:", ps,":total features = ", total, "\n")
      }
    }
    features$Serial = NULL
    viennaData = viennaRNADataManipulation(sequences)
    features = data.frame(features, viennaData)
    cat("2 viennaRNAdata related features :total features = ", (total + 2), "\n")
    return(features)
  }