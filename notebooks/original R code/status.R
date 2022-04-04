#'  Description to be added
#'
#' @param cases explanation of argument
#' @param w_s explanation of argument
#' @param ratio explanation of argument
#'
#'
#' @examples
#' status(cases, w_s, ratio)
#'
#' @export
status = function(cases, w_s, ratio) {
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
