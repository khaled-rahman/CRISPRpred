#' Illustration of countpattern
#
#' This function takes sequence and pattern as input and count how many times a particular pattern is present in the sequence.
#'
#' @param sequence provided as a list of sequences
#' @param pattern a string
#' @return a list of integer indicating frequency of pattern.
#' @export
#' @examples
#' sequence = list("ABDEFGHABDAACBBDEBGGGHHH", "ABCBDBEBEBBBDBDBFDFDFGGHHEEFFEECCCD")
#' pattern = "BD"
#' feat = featurization(sequence, pattern)
#' feat
countpattern <- function(sequence, pattern) {
  unlist(lapply(sequence,function(s)
    if(-1 %in% gregexpr(pattern = pattern, toString(s))[[1]]){0}else{length(
      gregexpr(pattern = pattern, toString(s))[[1]])}
    ))
}