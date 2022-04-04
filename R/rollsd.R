#' Rolling standard deviation
#'
#' This function computes the rolling standard deviation for a time series.
#' 
#' @return 
#' Returns a vector with the estimated rolling standard deviation for a time series.
#'
#' @param cases the time series of the newly observed cases per unit of time (ideally per day).
#' @param lag_t integer - the size of the rolling window for which the rolling standard deviation is calculated (minimum/default value = 7, maximum recommended value = 30).
#'
#' @examples
#' data("Italy")
#' cases = mova(cases=Italy$Cases, r_a = 7)
#' roll = rollsd(cases=cases,lag_t = 7)
#'
#' @references
#' Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021). \doi{10.1038/s41598-021-02622-3}
#'
#'
#' @export
rollsd = function(cases, lag_t=7) {
  rollsd=rep(NA,length(cases))
  for (i in 1:lag_t){
    rollsd[i]=medvol(cases[1:i])
  }
  for (i in (lag_t+1):length(cases)){
    rollsd[i]=medvol(cases[(i-(lag_t-1)):i])
  }
  return(rollsd)
}

