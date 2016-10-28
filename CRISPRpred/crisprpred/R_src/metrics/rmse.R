#' Root Mean Square Error
#' 
#' Return square root of mean squared error.
#' 
#' @param error a value denoting error
#'
#' @return rmse-error
#' @export
#' @examples
#' rmse(5)
rmse = function(error)
{
  sqrt(mean(error^2))
}
