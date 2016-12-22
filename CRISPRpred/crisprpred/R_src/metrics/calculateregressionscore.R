#' Illustration of calculateregressionscore
#'
#' This function takes dataset of all methods as input and produce mean value. It also draws boxplots.
#'
#' @param name a name of the regression method
#' @param data a dataframe which contains values of all methods
#' @return a list of mean values for all methods
#' @export
#' @examples
#' A = c(2,2.5,3.5,3)
#' B = c(4,4.5,5.4,4.6)
#' C = c(8.5,4.5,6.6,5.7)
#' data = data.frame(A,B,C)
#' features = calculateregressionscore("Simple Method", data)
calculateregressionscore = function(name = "Temporary", data) {
  par(mar = c(4, 5, 1.98, 1))
  boxplot(
    data, main = name, xlab = "Methods", ylab = "Spearman Correlation" , xaxis.lwd = 4, cex.main = 1.7, cex.lab = 1.7, lwd = 4, cex.axis = 2, boxwex = 0.4
  )
  box(lwd = 4)
  return(colMeans(data))
}