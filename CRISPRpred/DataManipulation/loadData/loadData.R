#'Description of loadData Function
#'
#'This function takes a full path of .csv file nas input which must be in current workspace and a a list of features. It stores .rda file in data folder.
#' @param filepath a full path of csv file
#' @param featurelist a list of features
#' @return none
#' @export
#' @examples
#' featurelist = c("X30mer", "Percent.Peptide", "Amino.Acid.Cut.position", "G_20", "GC_Count","predictions")
#' #suppose we have a file as '../crisprpred/data-raw/sample_data.csv' and current directory is set to '../crisprpred'
#' dir = getwd()
#' filepath = paste0(dir,'/data-raw/sample_data.csv')
#' loadData(filepath, featurelist)
loadData = function(filepath, featurelist){
dir = getwd()
viennaRNADataManipulation(filepath)
input = read.csv(filepath)
#featuredata = featurization(input, featurelist)
#print(input)
splitpath = unlist(strsplit(filepath, split="/"))
filename = splitpath[length(splitpath)]
rdafile = substr(filename,1,nchar(filename)-4)
#print(dir)
subdir = "data"
ifelse(!dir.exists(file.path(dir, subdir)), dir.create(file.path(dir, subdir)), FALSE)
rdafilepath = paste0(dir,"/data/",rdafile,".rda")
#print(rdafilepath)
save(input, file = rdafilepath)
#write.csv(featuredata, filepath)
}
