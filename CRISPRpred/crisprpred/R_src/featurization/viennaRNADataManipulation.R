#'Description of viennaRNADataManipulation Function
#'
#'This function takes a list of sequences as input, manipulates data using rna sequence of viennaRNA and returns a dataframe.
#' @param sequences a list of sequence strings
#' @return datafram of extracted features based on input sequences
#' @export
#' @examples
#' s = c('AGGCGTGTTAACT','ACGTTTAAGCT')
#' viennaRNADataManipulation(s)
viennaRNADataManipulation = function(sequences) {
  sgrna = sequences
  sgrna = unlist(lapply(sgrna, function(s)
    toString(s)))
  write.csv(sgrna, file = "in.csv")
  system("RNAfold < in.csv > out.csv")
  tempdata = read.csv("out.csv")
  system("rm out.csv")
  system("RNAheat --Tmin=50 --Tmax=50 < in.csv > out.csv")
  heatdata = read.csv("out.csv")
  tempdata = unlist(lapply(tempdata$X, function(s)
    toString(s)))
  mfe = unlist(lapply(tempdata, function(s)
    if (nchar(s) > 9) {
      t = unlist(strsplit(s, " "));
      as.numeric(gsub("[()]","",t[length(t)]))
    }else{
    }))
  #heatdata = unlist(lapply(heatdata, function(s)
  #  toString(s)))
  heat = unlist(lapply(heatdata[,1], function(x){t = unlist(strsplit(toString(x)," "));as.numeric(t[length(t)])}))
  mfe[1] = NA
  #heat[1] = NA
  mfe = mfe[!is.na(mfe)]
  heat = heat[!is.na(heat)]
  system("rm in.csv")
  system("rm out.csv")
  system("rm rna.ps")
  #cat("mfe:",length(mfe)," heat:",length(heat),"\n")
  vdata = data.frame(mfe,heat)
  return(vdata)
}
