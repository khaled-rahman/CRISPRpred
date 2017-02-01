## This file generate sgrna to integer code feature as well as features from Doench et al.

#data <- read.csv("FC_plus_RES.csv")
geticode = function(sgrnalist) {
  icode = c()
  for (i in 1:length(sgrnalist)) {
    s <- toString(sgrnalist[i])
    code = 0
    for (j in 5:24) {
      if(substr(s,j,j) == 'A'){
        code = code + (2^(j-5)) * 1
      }
      else if(substr(s,j,j) == 'C'){
        code = code + (2^(j-5)) * 2
      }
      else if(substr(s,j,j) == 'G'){
        code = code + (2^(j-5)) * 3
      }
      else if(substr(s,j,j) == 'T'){
        code = code + (2^(j-5)) * 4
      }
    }
    code = code / (8*(2^20 - 1))
    icode = c(icode, code)
    cat(s, ": ", code, "\n")
  }
  return (icode)
}