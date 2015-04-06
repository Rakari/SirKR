#' Sum of Vector Elements
#' 
#' Takes in a numeric vector and returns the sum of it. If at least one entry is numeric, then the NA entries are treated as zeros. If no entry is a value, then it returns NA.
#' @param x A numeric vector to find the mean of
#' @return The sum of the vector x
#' @examples 
#' x <- c(5,3,7,2,8,5,6,4,NA,NA,3,2,NA,4)
#' sumNA(x) 
#' returns: 49
#' @export

sumNA <- function(x) {
    if(!(class(x) %in% c("numeric", "logical"))) {
        stop("The input value is not able to convert to a numeric vector")
    }
    
    returnValue = 0
    
    if(all(is.na(x)) == TRUE) {
        returnValue = sum(x, na.rm = F)
    } else {
        returnValue = sum(x, na.rm = T)
    }
    
    returnValue
}