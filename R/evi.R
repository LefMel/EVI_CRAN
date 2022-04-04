#' Calculation of the Epidemic Volatility Index
#'
#' Calculates the relative change in the standard deviation between two consecutive rolling windows.
#' 
#' @return 
#' Returns a vector of the relative changes in the standard deviation between two consecutive rolling windows for a time series.  
#'
#' @param rollsd numeric vector - returned and stored as roll from the rollsd() function.
#'
#'
#' @examples
#' data("Italy")
#' cases = mova(cases = Italy$Cases)
#' roll = rollsd(cases = cases)
#' ev = evi(rollsd = roll)
#'
#' @export
#' 
#' @references
#' Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021). \doi{10.1038/s41598-021-02622-3}


evi = function(rollsd) {
  evi=rep(NA,length(rollsd))
  for (i in 2:length(rollsd)){
    evi[i]= (rollsd[i]-rollsd[i-1])/rollsd[i-1]
  }
  evi[is.nan(evi)]<-0
  return(evi)
}



