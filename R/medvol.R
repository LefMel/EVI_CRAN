#' Standard deviation
#'
#' This function calculates the standard deviation of a vector.

#' @return
#' Returns the standard deviation of a vector.
#' 
#' @param x numeric vector
#'
#'
#' @examples
#' x = c(2,2,1,4,9,10,23,10,9,10,14,12,10)
#' medvol(x)
#'
#' @export
#' 
#' @references
#' Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021). \doi{10.1038/s41598-021-02622-3}
#'
medvol=function(x){
  sdx<-sd(x)
  return(sdx)
}


