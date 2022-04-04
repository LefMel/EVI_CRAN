#'  Description to be added
#'
#' @param rollsd explanation of argument
#'
#'
#' @examples
#' evi(x)
#'
#' @export
evi = function(rollsd) {

  evi=rep(NA,length(rollsd))
  for (i in 2:length(rollsd)){
    evi[i]= (rollsd[i]-rollsd[i-1])/rollsd[i-1]
  }
  evi[is.nan(evi)]<-0
  return(evi)
}



