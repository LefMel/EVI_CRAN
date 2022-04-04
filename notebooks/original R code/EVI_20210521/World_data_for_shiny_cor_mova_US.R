library(readr)
library(tidyr)
library(dplyr)



wdata=read.csv(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

wdata=as.data.frame(wdata)
wdata$Province_State=as.factor(wdata$Province_State)
wdata$loc_n=as.numeric(wdata$Province_State)
nn=length(levels(wdata$Province_State))
last_c=ncol(wdata)-1
wdata_l <- gather(wdata, condition, measurement, 12:last_c, factor_key=TRUE)
wdata_l$condition=sub("X","", wdata_l$condition)

wdata_l$condition=as.Date(wdata_l$condition, "%m.%d.%y")

nam=rep(0, nn)
for(i in 1:nn) {
  
  wdata_ll=wdata_l[which(wdata_l$loc_n==i),]
  nam[i] <- paste(wdata_ll$Province_State[1], ".csv", sep = "")
  
  wdata_ll=select(wdata_ll,condition, measurement)
  wdata_ll$condition=as.numeric(wdata_ll$condition)
  wdata_ll=aggregate(. ~ condition, data=wdata_ll, FUN=sum)
  
  wdata_ll$ncases=rep(0,nrow(wdata_ll))
  
  wdata_ll$ncases[1]=wdata_ll$measurement[1]
  
  for (j in 2:nrow(wdata_ll)){
    wdata_ll$ncases[j]=wdata_ll$measurement[j]-wdata_ll$measurement[j-1]
  }
  
  plot(wdata_ll$ncases, type="l")
  
  wdata_ll=head(wdata_ll,-1)
  
  write.table(wdata_ll, file=file.path("W_cc_US",nam[i]), sep=";", append=FALSE , 
              quote=F, row.names=FALSE, col.names=T)
  
  
}


#setwd("Downloads/W_countries")
names=nam
  #gsub(".csv","",nam)
nn=length(names)





for(k in 1:nn) {
  
  datal <- read_delim(paste("W_cc_US/",names[k], sep=""),";",escape_double = FALSE, trim_ws = TRUE)
  namesa <- paste("results", names[k], sep = "")
  print(namesa)
  
  #print(plot(datal$ncases, type="l", main=namesa))
  
  
  
  #.rs.restartR()
  #rm(list=ls())
  
  
  mova=function(cases, int=7){
    ncases=rep(NA, length(cases))
    for (i in 1:length(cases)){
      ncases[i]=mean(cases[((i+1)-min(int,i)):i])
    }
    return(ncases)
  }
  
  medvol=function(x){
    return(sd(x))
  }
  
  #rolling sd
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
  
  
  #evi calcuation
  evi = function(rollsd) {
    
    evi=rep(NA,length(rollsd))
    for (i in 2:length(rollsd)){
      evi[i]= (rollsd[i]-rollsd[i-1])/rollsd[i-1]
    }
    evi[is.nan(evi)]<-0
    return(evi)
  }
  
  #index calculation
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
  
  #test which cut-off c & lag
  evifcut = function(evi, cases, rate, w_s, ratio) {
    
    test_p=rep(NA, length(cases))
    true_p=rep(NA, length(cases))
    
    
    
    for (i in w_s:(length(cases)-w_s)){
      if (evi[i]>=rate && cases[i]>mean(cases[i:(i-7)]))
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
  
  
  
  #desired detection difference
  ratio=1/1.2
  w_s=7
  
  #select the new confirmed cases
  nncases=datal$ncases
  
  #calculate the moving average of new confrimed cases
  cases=mova(nncases)
  
  #first observations ("burn-in") of the time series
  start_cases=14
  
  #intial values for the window and c for
  #the analysis of the first observations 
  lag_1=7
  c_1=0.01
  
  
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
    lag_s=seq(lag_1,min(30,(length(case_t)-1)), 1)
    #lag_s=seq(lag_1,min(length(case_t),50), 1)
    c_s=seq(0.01,0.5, 0.01)
    
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
  dates=datal$condition
  
  
  data=as.data.frame(cbind(dates,Days, EVI, Cases, Index, pvs, pvn, 
                           lag_all, c_all, se_all, sp_all))
  
  
  
  write.table(data, file=paste("forplot_corr_US/",namesa, sep=""), sep=",", append=F , 
              quote=F, row.names=FALSE, col.names=T)
  
}




