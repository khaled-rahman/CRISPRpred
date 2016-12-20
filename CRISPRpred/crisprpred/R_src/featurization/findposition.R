#' Illustration of findposition
#
#' This function takes sequence, pattern and position as input and check whether a particulaer pattern is present in position-th place of sequence.
#'
#' @param sequence provided as a list of sequences
#' @param pattern a string
#' @param position an integer value
#' @return a list of 0/1 indicating present or absent.
#' @export
#' @examples
#' sequence = list("ABDEFGHABDAACBBDEBGGGHHH", "ABCBDBEBEBBBDBDBFDFDFGGHHEEFFEECCCD")
#' pattern = "BD"
#' position = 2
#' feat = featurization(sequence, pattern, position)
#' feat

findposition <- function(sequence, pattern, position) {
  unlist(lapply(sequence,function(s)
    if(position %in% gregexpr(pattern = pattern, toString(s))[[1]]){1}else{0}
  ))
}