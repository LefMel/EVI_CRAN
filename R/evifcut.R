#' Sensitivity-Specificity estimation for each cut-off value and rolling window size
#'
#' This function calculates the sensitivity and the specificity for each cut-off value and rolling window size.
#'
#' @return
#' Returns a list of the estimated Sensitivity, Specificity, apparent and true prevalence for each cut-off value and rolling window size
#' 
#' @param evi numeric vector - object (obtained from the evi function and stored as ev) that corresponds to the relative change in the standard deviation.
#' @param cases numeric vector with the number of new cases per unit of time (i.e., daily).
#' @param cut threshold value (0 <= c <= 0.5) for issuing an early warning. If evi >= c, an early warning is issued and otherwise is not.
#' @param r Definition for the minimum difference in the mean number of cases, one week before and after each time point that, if present, should be detected. This is the case definition and the default is 0.2 (with 0 <= r <= 1). A value of r=0.2 means that we have a case when the mean number of the newly observed cases in the next 7 days is at least 20% higher than the mean number of the newly observed cases in the past 7 days.
#'
#' @examples
#' data("Italy")
#' cases = mova(cases = Italy$Cases)
#' roll = rollsd(cases = cases)
#' ev = evi(rollsd = roll)
#' evifcut(evi = ev, cases = cases, cut = 0.01, r = 0.2)

#' @export
#' 
#' @references
#' Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021). \doi{10.1038/s41598-021-02622-3}


evifcut = function(evi, cases, cut, r) {

  w_s=7
  ratio = 1/(1+r)
  test_p=rep(NA, length(cases))
  true_p=rep(NA, length(cases))



  for (i in w_s:(length(cases)-w_s)){
    if (evi[i]>=cut && cases[i]>mean(cases[i:(i-6)]))
    {test_p[i]=1}
    else
    {test_p[i]=0}

    if (mean(cases[(i-(w_s-1)):i])<=ratio*mean(cases[(i+1):(i+w_s)]))
    {true_p[i]=1}
    else
    {true_p[i]=0}

  }

  sens=length(which(test_p==1 & true_p==1))/length(which(true_p==1))
  spec=length(which(test_p==0 & true_p==0))/length(which(true_p==0))
  sens[is.nan(sens)]<-0
  spec[is.nan(spec)]<-0
  testsin=length(which(test_p==1))/(length(cases)-w_s)
  prev=length(which(true_p==1))/(length(cases)-w_s)

  evifcut<-list(sens=sens, spec=spec, testsin=testsin, prev=prev)
  return(evifcut)
}



