#'Description of loadData Function
#'
#'This function takes the full path of a .csv file as input which must be in current workspace, manipulates data using rna sequence of viennaRNA and stores that in the save  .csv file in data folder.
#' @param filepath a full path of csv file
#' @return none
#' @export
#' @examples
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample_data.csv')
#' viennaRNADataManipulation(filepath)
viennaRNADataManipulation = function(filepath){
#filename = "sample_data.csv"
dir = getwd()
#print(dir)
#filepathc = paste0(dir,"/data-raw/",filename)
data = read.csv(filepath)
sgrna = data["X30mer"][[1]]
sgrna = unlist(lapply(sgrna, function(s)
  toString(s)))
write.csv(sgrna, file = "in.csv")
system("RNAfold < in.csv > out.csv")
tempdata = read.csv("out.csv")
#print(dat$X)
tempdata = unlist(lapply(tempdata$X, function(s)
  toString(s)))
mfe = unlist(lapply(tempdata, function(s) if(nchar(s)>9){t = unlist(strsplit(s, " "));
as.numeric(substr(t[length(t)], 1, nchar(t[length(t)]) - 1))}else{}))
#print(mfe)
mfe[1] = NA
mfe = mfe[!is.na(mfe)]
if(!"MFE" %in% colnames(data)){
data["MFE"] = mfe
#print(mfe)
#writepath = paste0(dir,"/data-raw/",filename)
write.csv(data, file = filepath, row.names = FALSE)
}
system("rm in.csv")
system("rm out.csv")
system("rm rna.ps")
}
