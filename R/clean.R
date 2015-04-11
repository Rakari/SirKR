#'Clean Algae dataset
#'
#' Takes in a data frame and cleans it properly for use in the algaeplot
#' @param df The data frame to be cleaned
#' @return The cleaned data frame
#' @export

clean <- function(df) {
     zerodf = data.frame(matrix(0, dim(df)[1], dim(df)[2]))
     
     colnames(zerodf) = names(df)
     
     out.df = rbind(df,zerodf)
     
     n = nrow(zerodf)
     out.df = out.df[kronecker(1:n, c(n,0), "+"),]
     out.df[nrow(out.df)+1,] = rep(0,ncol(out.df))
     
     rownames(out.df) = 1:nrow(out.df)
     
     for(j in 2:ncol(out.df)) {
          for(i in 1:nrow(out.df)) {
               value=out.df[i,j]
               
               if(!is.na(value)) {
                    if(value == 0) {
                         if(i == 1) {
                              if(is.na(out.df[i+1,j])) {
                                   out.df[i,j] = NA
                              }
                         } else if(i == nrow(out.df)) {
                              if(is.na(out.df[i-1,j])) {
                                   out.df[i,j] = NA
                              }
                         } else if(is.na(out.df[i-1,j]) & !is.na(out.df[i+1,j]) | !is.na(out.df[i-1,j]) & is.na(out.df[i+1,j])) {
                              out.df[i,j] = 0
                         } else {
                              out.df[i,j] = mean(c(out.df[i-1,j], out.df[i+1, j]))
                         }
                    }
               }
          }
     }
     
     out.df[,1] = 1:nrow(out.df)
     
     out.df
}