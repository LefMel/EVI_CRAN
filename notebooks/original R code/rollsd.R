#'  Description to be added (rolling sd)
#'
#' @param cases explanation of argument
#' @param lag_t explanation of argument
#'
#' @examples
#' cases=c(2,2,1,4,9,10,23,10,9,10,14,12,10)
#' rollsd(cases,lag_t)
#'
#' @export
rollsd = function(cases, lag_t) {
  rollsd=rep(NA,length(cases))
  for (i in 1:lag_t){
    rollsd[i]=medvol(cases[1:i])
  }
  for (i in (lag_t+1):length(cases)){
    rollsd[i]=medvol(cases[(i-(lag_t-1)):i])
  }
  return(rollsd)
}


rollsd(rbinom(100,10,0.5),lag_t=20)
