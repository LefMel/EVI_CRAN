evifcut_cEVI<-function (evi, cases, r)
{
  w_s = 7
  ratio = 1/(1 + r)
  test_p = rep(NA, length(cases))
  true_p = rep(NA, length(cases))

  for (i in w_s:(length(cases)-w_s)){

    if ((!is.na(evi[i]) & evi[i]  == 1) && cases[i]>mean(cases[i:(i-7)]))
    {test_p[i] <- 1}else{test_p[i] <- 0}

    if (mean(cases[(i):(i-w_s+1)]) <= ratio * mean(cases[(i+1):(i+w_s)],na.rm=T))
    {true_p[i] <- 1}else{true_p[i] <- 0}
  }

  sens = length(which(test_p == 1 & true_p == 1))/length(which(true_p ==
                                                                 1))
  spec = length(which(test_p == 0 & true_p == 0))/length(which(true_p ==
                                                                 0))

  sens[is.nan(sens)] <- 0
  spec[is.nan(spec)] <- 0
  testsin = length(which(test_p == 1))/(length(cases) - w_s)
  prev = length(which(true_p == 1))/(length(cases) - w_s)
  evifcut <- list(sens = sens, spec = spec, testsin = testsin,
                  prev = prev)
  return(evifcut)
}
