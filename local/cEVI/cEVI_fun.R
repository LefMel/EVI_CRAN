cEVI_fun<-function(cases,lag_n,c_n){

cevi <- rep(NA, length(cases))
for(k in (lag_n+1):(length(cases)-(lag_n+1))){
  enu=mean(cases[(k+1):(k+lag_n)]-cases[(k):(k-(lag_n-1))],na.rm = T)
  den1=sd(cases[(k):(k-(lag_n-1))])^2/(length(cases[(k):(k-(lag_n-1))]))
  den2=sd(cases[(k+1):(k+lag_n)])^2/(length(cases[(k+1):(k+lag_n)]))

  # Spectral variances more appropriate but more time consuming
  #den1=spectrum0.ar(cases[(i+1):(i+w_s)])$spec/(length(cases[(i+1):(i+w_s)])) # Spectral variances
  #den2=spectrum0.ar(cases[(i):(i-(w_s-1))])$spec/(length(cases[(i):(i-(w_s-1))]))
  test=enu/sqrt(den1+den2)
  cevi[k+lag_n+1]=as.numeric((1-pnorm(test))<=c_n)#*as.numeric(evi[i] >= rate)
}
return(cevi)
}
