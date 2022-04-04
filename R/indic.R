#' Issue of an Early Warning 
#' 
#' This function produces the early warning signal (Index).
#'
#' @return
#' A vector of 0s and 1s is produced, where a 1 (Index = 1) is recorded when an early warning is issued and a 0 (Index = 0) when an early warning is not issued.
#'
#' @param evi numeric vector - object (obtained from the evi function and stored as ev) that corresponds to the relative change in the standard deviation.
#' @param cut threshold value (0 <= c <= 0.5) for issuing an early warning. If evi >= c an early warning is issued and otherwise is not.
#' @param cases numeric vector with the number of new cases per unit of time (i.e., daily).
#'
#' @examples
#' data("Italy")
#' cases = mova(cases = Italy$Cases, r_a = 7)
#' roll = rollsd(cases = cases, lag_t = 7)
#' ev = evi(rollsd = roll)
#' ind=indic(evi = ev, cut = 0.01, cases = cases)
#'
#' @export
#' 
#' @references
#' Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021). \doi{10.1038/s41598-021-02622-3}

indic = function(evi, cut, cases) {
  ind=rep(NA,length(evi))
  for (i in 3:length(evi)){
    if (evi[i]>=cut && cases[i]>mean(cases[i:(i-min(7,i))]))
    {ind[i]=1}
    else
    {ind[i]=0}
  }
  return(ind)
}
