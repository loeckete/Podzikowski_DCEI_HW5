#' Standard Error Function
#'
#' This function allows you to calculate the standard error of a sample
#' @param x a vector of numbers
#' @examples
#' x<-c(1,2,3,4,5,6,7,8,9,10,11)
#' se(x)

se<-function(x){sd(x)/sqrt(length(x))}

#' Number of Observations Function
#'
#' This function allows you to calculate the number of observation in a set
#' @param x a vector
#' @examples
#' x<-c(1,2,3,4,5,6,7,8,9,10,11)
#' N(x)
#'
#' x<-c("a","b","c")
#' N(x)

N<-function(x){length(x)}

#' Upper Fence Function
#'
#' This function allows you to calculate the upper fence (Tukey Method) for determining outliers.  Where the UF = Q3 + n(Q3-Q1).
#' @param x a vector of numbers
#' @param n is a constant, default is 1.5, n is often set to 3 to determine extreme outliers assuming a normal distribution.
#' @examples
#' x<-c(1,2,3,4,5,6,7,8,9,10,11)
#' UF(x)

UF<-function(x,n=1.5){
  #UF stands for upper fence, from boxplot method for ident. outliers
  UF<-((quantile(x,0.75)-quantile(x,0.25))*n)+quantile(x,0.75)
  return(UF)
}

#' Lower Fence Function
#'
#' This function allows you to calculate the lower fence (Tukey Method) for determining outliers.  Where the UF = Q1 - n(Q3-Q1).
#' @param x a vector of numbers
#' @param n is a constant, default is 1.5, n is often set to 3 to determine extreme outliers assuming a normal distribution.
#' @examples
#' x<-c(1,2,3,4,5,6,7,8,9,10,11)
#' UF(x)

LF<-function(x,n=1.5){
  #LF stands for upper fence, from boxplot method for ident. outliers
  LF<-quantile(x,0.25)-((quantile(x,0.75)-quantile(x,0.25))*n)
  return(LF)
}


