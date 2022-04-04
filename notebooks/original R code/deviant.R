deviant=function(new_cases, ratio=1/1.2, lag_max=30, start_cases=14, lag_1=7, c_1=7){
  source("mova.r")
  source("medvol.r")
  source("evi.r")
  source("evifcut.r")
  source("indic.r")
  source("status.r")
  
  #desired detection difference
  w_s=7 # Na paei pano!########################
  
  
  #calculate the moving average of new confrimed cases
  cases=mova(new_cases)
  
  
  roll=rollsd(cases[1:start_cases],lag_1)
  ev=evi(roll)
  ind=indic(ev,c_1, cases[1:start_cases])
  status=status(cases[1:start_cases],w_s,ratio)
  
  #initiate chain for positive predictive value
  pvs=rep(NA, length(cases))
  
  #initiate chain for negative predictive value
  pvn=rep(NA, length(cases))
  
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
    lag_s=seq(lag_1,min(lag_max,(length(case_t)-1)), 1)
    #lag_s=seq(lag_1,min(length(case_t),50), 1)
    
    
    
    c_s=seq(0.01,0.5, 0.01) # Na paei pano! #########################3
    
    
    
    #all_j=NA
    
    all_lag=NA
    all_cut=NA
    all_se=NA
    all_sp=NA
    
      
      
      for (j in lag_s){
        roll_t=rollsd(case_t,j)
        ev_t=evi(roll_t)
        for (l in c_s){
          evicut_t=evifcut(ev_t, case_t, l, w_s, ratio)
          new_j=j
          new_l=l
          new_se=evicut_t$sens
          new_sp=evicut_t$spec
          all_lag[[length(all_lag) + 1]] <- new_j
          all_cut[[length(all_cut) + 1]] <- new_l
          all_se[[length(all_se) + 1]] <- new_se 
          all_sp[[length(all_sp) + 1]] <- new_sp
          
          
        }
      }
      
      
      
      sesp=as.data.frame(cbind(all_lag,all_cut,all_se,all_sp))
      
      
      
      
      #Select the row with the right window and cut
      index=which.max(sesp$all_se+sesp$all_sp-1)
      
      #index=sesp[which(sesp$all_sp>0.80),]
      #index=which.max(index$all_se)
      #index=which(sesp$all_se==1 & sesp$all_sp>=0.95),1)
      #if (i>40) 
      #   {index1=sesp[which(sesp$all_sp>0.95),]
      #  index=which.max(index1$all_se)
      #   }
      #else
      #{index=which.max(sesp$all_se+sesp$all_sp-1)}
      
      
      #index=which(sesp$se>=0.6 & sesp$sp>0.9)
      print(i)
      print(sesp[index,])
      
      
      
      #estimate the parameters for the last observed case
      lag_n=sesp$all_lag[index]
      c_n=sesp$all_cut[index]
      
      roll_n=rollsd(cases[1:i],lag_n)
      
      ev_n=evi(roll_n)
      ind_n=indic(ev_n,c_n, case_t)
      evicut_n=evifcut(ev_n, case_t, c_n, w_s, ratio)
      
      roll=c(roll,roll_n[i])
      ev=c(ev,ev_n[i])
      ind=c(ind, ind_n[i])
      
      lag_all=c(lag_all,lag_n)
      c_all=c(c_all,c_n)
      
      se_all=c(se_all,all_se[index])
      sp_all=c(sp_all,all_sp[index])
      
      pvs[i]=evicut_n$prev*all_se[index]/
        (evicut_n$prev*all_se[index]+(1-evicut_n$prev)*(1-all_sp[index]))
      
      pvn[i]=(1-evicut_n$prev)*all_sp[index]/
        ((1-evicut_n$prev)*all_sp[index]+evicut_n$prev*(1-all_se[index]))
      
      
      
    }
    
    
    Days=(1:length(cases))
    EVI=ev
    Cases=cases
    Index=ind
    
    
    EVI_out=as.data.frame(cbind(Days, EVI, Cases, Index, pvs, pvn, 
                                lag_all, c_all, se_all, sp_all))
    EVI_output<<-(EVI_out)
    
}

  
 
