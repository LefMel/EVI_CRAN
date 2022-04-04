#index calculation
indic_cEVI <- function(evi, cases) {
  ind <- rep(NA,length(evi))
  for (i in 3:length(evi)) {
    if (!is.na(evi[i]) && evi[i] ==1 && (!is.na(cases[i]) & cases[i] > mean(cases[i:(i - min(7, i))])))
    {ind[i] <- 1}
    else
    {ind[i] <- 0}
  }
  return(ind)
}
