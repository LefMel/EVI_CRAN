#' True status definition
#' 
#' This function defines the true status based on the case definition.
#'
#' @return
#' A vector of 0s and 1s is produced. Status = 1 is when the expected rise in the number of cases occurs and Status = 0 when the expected rise in the number of cases does not occur.
#'
#' @param cases the time series of the newly observed cases per unit of time (ideally per day).
#' @param r	 Definition for the minimum difference in the mean number of cases, one week before and after each time point that, if present, should be detected. This is the case definition and the default is 0.2 (with 0 <= r <= 1). A value of r=0.2 means that we have a case when the mean number of the newly observed cases in the next 7 days is at least 20% higher than the mean number of the newly observed cases in the past 7 days.
#'
#' @examples
#' data("Italy")
#' cases = mova(cases=Italy$Cases)
#' status = status(cases=cases, r=0.2)
#' @export
#'
#' @references
#' Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021). \doi{10.1038/s41598-021-02622-3}
#'
#'
status = function(cases, r) {
  ratio = 1/(1+r)
  w_s=7
  status=rep(NA,length(cases))
  status[1]=NA
  for (i in 2:(length(cases)-w_s)){
    if (mean(cases[(i-min((i-1),w_s)):(i-1)])<=ratio*mean((cases[i:(i+min(i,(w_s-1)))])))
    {status[i]=1}
    else
    {status[i]=0}
  }
  return(status)
}
