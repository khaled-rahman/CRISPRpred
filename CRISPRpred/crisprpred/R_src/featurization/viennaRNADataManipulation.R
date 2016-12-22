#'Description of viennaRNADataManipulation Function
#'
#'This function takes a list of sequences as input, manipulates data using rna sequence of viennaRNA and returns a dataframe.
#' @param sgrna a list of sequence strings
#' @return datafram of extracted features based on input sequences
#' @export
#' @examples
#' s = c('AGGCGTGTTAACT','ACGTTTAAGCT')
#' viennaRNADataManipulation(s)
viennaRNADataManipulation = function(sgrna) {
  sgrna = unlist(lapply(sgrna, function(s)
    toString(s)))
  write.csv(sgrna, file = "in.csv")
  if (.Platform$OS.type == "unix") {
    system("RNAfold < in.csv > out.csv")
    system("RNAheat --Tmin=50 --Tmax=50 < in.csv > heat.csv")
    tempdata = read.csv("out.csv")
    heatdata = read.csv("heat.csv")
    system("rm in.csv")
    system("rm out.csv")
    system("rm rna.ps")
    system("rm heat.csv")
  }else{
    system("cmd.exe",input = "RNAfold < in.csv > out.csv")
    system("cmd.exe",input = "RNAheat --Tmin=50 --Tmax=50 < in.csv > heat.csv")
    tempdata = read.csv("out.csv")
    heatdata = read.csv("heat.csv")
    system("cmd.exe",input = "rm in.csv")
    system("cmd.exe",input = "rm out.csv")
    system("cmd.exe",input = "rm rna.ps")
    system("cmd.exe",input = "rm heat.csv")
  }
  tempdata = unlist(lapply(tempdata$X, function(s)
    toString(s)))
  mfe = unlist(lapply(tempdata, function(s)
    if (nchar(s) > 9) {
      t = unlist(strsplit(s, " "));
      as.numeric(gsub("[()]","",t[length(t)]))
    }else{
      
    }))
  
  heat = unlist(lapply(heatdata[,1], function(x) {
    t = unlist(strsplit(toString(x)," "));as.numeric(t[length(t)])
  }))
  mfe[1] = NA
  mfe = mfe[!is.na(mfe)]
  heat = heat[!is.na(heat)]
  vdata = data.frame(mfe,heat)
  return(vdata)
}
