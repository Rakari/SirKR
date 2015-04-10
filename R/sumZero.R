sumZero <- function(x) {
     if(!(class(x) %in% c("numeric", "logical"))) {
          stop("The input value is not able to convert to a numeric vector")
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