#' Illustration of Featurization
#'
#' This function takes dataset and a list of features as input and produce a features-wise dataset. The number of columns in returned dataset is equal to the number of features in featurelist.
#'
#' @param data provided dataset from csv file
#' @param featurelist a list of strings
#' @return a feature-wise dataset
#' @export 
#' @examples 
#' featurelist = c("X30mer", "Percent.Peptide", "Amino.Acid.Cut.position", "G_20", "GC_Count","predictions")
#' featuredata = featurization(input, featurelist)
#' featuredata
featurization = function(data, featurelist) {
  #print(featurelist)
  sgrna = data["X30mer"][[1]];
  features = unlist(lapply(featurelist, function(f)
    if (f == "G_20") {
      presence = unlist(lapply(sgrna, function(s)
        if(20 %in% gregexpr(pattern = "G",toString(s))[[1]]) {
          1} else {0}));
      data["G_20"] <<- presence;
      #cat("G_20:",presence,"\n")
    } else{
      if (f == "GC_Count") {
        count = unlist(lapply(sgrna,function(s) (length(gregexpr(pattern = "G", toString(s))[[1]]) + length(gregexpr(pattern = "C", toString(s))[[1]]))));
        #cat("GC_Count:", count,"\n")
        data["GC_Count"] <<- count;
        #print(data)
      } else{
        if(f == "AA_18"){
          presence = unlist(lapply(sgrna, function(s)
            if ((18 %in% gregexpr(pattern = "A",toString(s))[[1]]) && (19 %in% gregexpr(pattern = "A",toString(s))[[1]])) {1}else{0}));
          #cat("In AA_18:","\n")
          data["AA_18"] <<- presence;
        } else{
          if(f == "TT_17"){
            presence = unlist(lapply(sgrna, function(s)
              if ((17 %in% gregexpr(pattern = "T",toString(s))[[1]]) && (18 %in% gregexpr(pattern = "T",toString(s))[[1]])) {1} else{0}));
            data["TT_17"] <<- presence;    
            #cat("In TT_17 \n")
          } else {
            if(f == "A"){
              presence=unlist(lapply(sgrna, function(s)
                if (-1 %in% gregexpr(pattern = "A",toString(s))[[1]]) {0}else{1}));
              	#cat(presence);
		data["A"] <<- presence;
            }else{
		if(f == "GG"){
		  presence = unlist(lapply(sgrna, function(s)
		    if((-1 %in% gregexpr(pattern = "GG",toString(s))[[1]])){0}else{length(gregexpr(pattern = "GG",toString(s))[[1]])}));
		  data["GG"] <<- presence;
		  #cat("In GG\n");
		   }else{}
}}}}}));
  #print(features)
  #print(data)
  return (data)
}

