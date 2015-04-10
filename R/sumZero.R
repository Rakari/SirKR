#' Sum of Vector Elements
#' 
#' Takes in a numeric vector and returns the sum of it. If at least one entry is numeric, then the function returns 0. If no entry is a value, then it returns NA.
#' @param x A numeric vector to find the sum of
#' @return The sum of the vector x
#' @examples 
#' x <- c(NA,2)
#' sumZero(x) 
#' returns: 0
#' @export
sumZero <- function(x) {
     if(!(class(x) %in% c("numeric", "logical"))) {
          #stop("The input value is not able to convert to a numeric vector")
     }
     
     returnValue = 0
     
     if(all(is.na(x)) == TRUE) {
          returnValue = sum(x, na.rm = F)
     } else if(is.na(x[1]) & !is.na(x[2]) | !is.na(x[1]) & is.na(x[2])) {
          returnValue = 0
     } else {
          returnValue = sum(x, na.rm = T)
     }
     
     returnValue
}