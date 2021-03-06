% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/meanNA.R
\name{meanNA}
\alias{meanNA}
\title{Arithmetic Mean}
\usage{
meanNA(x)
}
\arguments{
\item{x}{A numeric vector to find the mean of}
}
\value{
The mean of the vector x
}
\description{
Takes in a numeric vector and returns the mean of it. If at least one entry is numeric, then the NA entries are treated as zeros. If no entry is a value, then it returns NA.
}
\examples{
x <- c(5,3,7,2,8,5,6,4,NA,NA,3,2,NA,4)
meanNA(x)
returns: 3.5
}

