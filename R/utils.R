
# dataRange -------------------------

#' @title dataRange function
#' @description Return the range of X for each variable.
#' @author Quan Gu
#' 
#' @usage dataRange(X)
#' @param X A matrix or a data frame of training data **without** y column.
#' 
#' @details Return the range of X for each variable, which is a part of \code{param}. Can be used for passing \code{param} to \code{Elem, ORT or ORF}.
#' @return A data frame with two columns \code{min max} contains the range of each x variable.
#' 
#' @examples
#' dataRange(iris[, 1:4])
#' dataRange(mtcars)
#'
#' @export

dataRange <- function(X) {
  stopifnot(is.matrix(X) | is.data.frame(X))
  data.frame(min = apply(X, 2, min),
             max = apply(X, 2, max),
             row.names = paste0("X", 1:ncol(X)))
}


