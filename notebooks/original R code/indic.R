#'  Description to be added
#'
#' @param evi explanation of argument
#' @param cut explanation
#' @param cases explanation
#'
#' @examples
#' medvol(x)
#'
#' @export
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
