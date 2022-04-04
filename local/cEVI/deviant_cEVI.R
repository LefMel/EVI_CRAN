deviant_plus=function(new_cases, cum = FALSE, r_a=7, r=0.2, lag_max=30){
  #source("mova.r")
  #source("medvol.r")
  #source("evi.r")
  #source("evifcut.r")
  #source("indic.r")
  #source("status.r")
  #source("rollsd.r")

  start_time = Sys.time()
  start_cases=18
  lag_1=3
  c_1=0.001
  w_s=7

  if (cum == TRUE) new_cases = c(new_cases[1], diff(new_cases))

  #calculate the moving average of new confirmed cases
  cases=mova(new_cases,r_a)
  #roll=rollsd(cases[1:start_cases],lag_1)
  #ev=evi(roll)
  cevi=cEVI_fun(cases = cases[1:(start_cases)],lag_n = lag_1,c_n = c_1)
  ind=indic_cEVI(cevi, cases[1:start_cases])
  status=status(cases[1:start_cases],r)

  #initiate chain for positive predictive value
  ppv=rep(NA, length(cases))
  #initiate chain for negative predictive value
  npv=rep(NA, length(cases))

  lag_all=rep(NA, start_cases)
  c_all=rep(NA, start_cases)
  se_all=rep(NA, start_cases)
  sp_all=rep(NA, start_cases)
  lag_all[1:start_cases]=lag_1
  c_all[1:start_cases]=c_1


  for (i in (start_cases+1): length(cases)){

    case_t=cases[1:i]
    #case_t=cases[max(1,(i-33)):i]
    #lag_s=7
    lag_s=seq(lag_1,min(lag_max,(length(case_t)-1)), 2)
    #lag_s=seq(lag_1,min(length(case_t),50), 1)
    c_s=seq(0.001,0.5, 0.06)
    #all_j=NA

    all_lag=NA
    all_cut=NA
    all_se=NA
    all_sp=NA

      for (l in c_s) {
        for (j in lag_s) {
          # roll_t <- rollsd(case_t,j)
          #  ev_t <- evi(roll_t)
          cevi <- rep(NA, length(cases))
          for(k in (j+1):(length(cases)-(j+1))){
            enu=mean(cases[(k+1):(k+j)]-cases[(k):(k-(j-1))],na.rm = T)
            den1=sd(cases[(k):(k-(j-1))])^2/(length(cases[(k):(k-(j-1))]))
            den2=sd(cases[(k+1):(k+j)])^2/(length(cases[(k+1):(k+j)]))

            # Spectral variances more appropriate but more time consuming
            #den1=spectrum0.ar(cases[(i+1):(i+w_s)])$spec/(length(cases[(i+1):(i+w_s)])) # Spectral variances
            #den2=spectrum0.ar(cases[(i):(i-(w_s-1))])$spec/(length(cases[(i):(i-(w_s-1))]))
            test=enu/sqrt(den1+den2)
            cevi[k+j+1]=as.numeric((1-pnorm(test))<=l)#*as.numeric(evi[i] >= rate)
          }
          evicut_t <- evifcut_cEVI(evi=cevi,cases = case_t, r = r)
          all_lag[[length(all_lag) + 1]] <- j
          all_cut[[length(all_cut) + 1]] <- l
          all_se[[length(all_se) + 1]] <- evicut_t[[1]]
          all_sp[[length(all_sp) + 1]] <- evicut_t[[2]]
        }
      }



    sesp=as.data.frame(cbind(all_lag,all_cut,all_se,all_sp))


    #Select the row with the right window and cut
    index=which.max(sesp$all_se+sesp$all_sp-1)

    print(i)
    print(sesp[index,])

    #estimate the parameters for the last observed case
    lag_n=sesp$all_lag[index]
    c_n=sesp$all_cut[index]

    # Fix final indicator based on cevi procedure.

    cevi=cEVI_fun(cases = cases[1:i],lag_n = lag_n, c_n = c_n)
    ind_n=indic_cEVI(evi = cevi,cases = case_t)
    evicut_n=evifcut_cEVI(cevi, case_t, r)

    ind=c(ind, ind_n[i])
    lag_all=c(lag_all,lag_n)
    c_all=c(c_all,c_n)

    se_all=c(se_all,all_se[index])
    sp_all=c(sp_all,all_sp[index])

    ppv[i]=evicut_n$prev*all_se[index]/
      (evicut_n$prev*all_se[index]+(1-evicut_n$prev)*(1-all_sp[index]))

    npv[i]=(1-evicut_n$prev)*all_sp[index]/
      ((1-evicut_n$prev)*all_sp[index]+evicut_n$prev*(1-all_se[index]))
  }

  Days=(1:length(cases))
  EVI=cevi
  Cases=cases
  Index=ind

  EVI_out=as.data.frame(cbind(Days, EVI, Cases, Index, ppv, npv,
                              lag_all, c_all, se_all, sp_all))
  EVI_output<<-(EVI_out)

  end_time = Sys.time()

  time_elapsed = end_time - start_time
  print(time_elapsed)
  return(EVI_output)

}
