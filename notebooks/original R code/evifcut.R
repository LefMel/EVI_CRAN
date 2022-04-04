#'  Description to be added (#test which cut-off c & lag)
#'
#' @param evi explanation of argument
#' @param cases explanation
#' @param rate explanation
#' @param w_s explanation
#' @param ratio explanation
##'
#' @examples
#' medvol(x)
#'
#' @export
evifcut = function(evi, cases, rate, w_s, ratio) {

  test_p=rep(NA, length(cases))
  true_p=rep(NA, length(cases))



  for (i in w_s:(length(cases)-w_s)){
    if (evi[i]>=rate && cases[i]>mean(cases[i:(i-6)]))
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

  return(list(sens=sens, spec=spec, testsin=testsin, prev=prev))
}
