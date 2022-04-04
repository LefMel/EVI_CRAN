#' This function produces the Epidemic Volatility Index based output data
#'
#' This is the main function of the \code{\link[EVI:EVI-package]{EVI-package}} that you should use to analyze a time series of observed cases per unit of time (ideally per day).
#'
#'
#' For each time point the stored variables are:
#'@return \itemize{
#' \item{Dates: the date for each time point (with origin 01-01-1970).}
#'
#'\item{Days: the serial number for each time point.}
#'
#'\item{EVI: the estimated EVI for each time point.}
#'
#'\item{Cases: the rolling average of the newly observed cases for each time point. A 7-day rolling average is calculated by default (i.e., r_a=7). The user has the option to change this by modifying r_a.}
#'
#'\item{Index: takes values 1 or 0 for the issuance of an early warning or not, respectively.}
#'
#'\item{ppv: the positive predictive value for each time point.}
#'
#'\item{npv: the negative predictive value for each time point.}
#'
#'\item{lag_all: the selected rolling window size for EVI calculation for each time point.}
#'
#'\item{c_all: the selected cut-off for issuing an early warning for each time point.}
#'
#'\item{se_all: the sensitivity (Se) of EVI up to this time point.}
#'
#'\item{sp_all: the specificity (Sp) of EVI up to this time point.}}



#' @param new_cases the time series of the newly observed cases per unit of time (ideally per day).
#' @param cum TRUE or FALSE; TRUE if the time series is recorded as the cumulative number of the reported cases and FALSE (the default) if newly reported cases per unit of time are recorded.
#' @param r_a The window size for the moving average that will be analyzed. If set to 1 the actual observations are analyzed. However, due to the variability of the reported cases between working days and weekends it is recommended that the 7-day moving average is analyzed (i.e. r_a = 7), which is the default for this argument. Users could prefer a longer interval of 14 days or one month (i.e., r_a=14 or 30, respectively).
#' @param r Definition for the minimum difference in the mean number of cases, one week before and after each time point that, if present, should be detected. This is the case definition and the default is 0.2 (with 0 <= r <= 1). A value of r=0.2 means that we have a case when the mean number of the newly observed cases in the next 7 days is at least 20% higher than the mean number of the newly observed cases in the past 7 days.
#' @param lag_max Integer. Restriction of the maximum window size for the rolling window size. The default is set to one month (lag_max=30) to prevent excess volatility of past epidemic waves from affecting the most recent volatility estimates and the ability of EVI to warn for upcoming waves that may be smaller and of lower volatility than previous ones.
#'
#'
#' @examples
#'      \dontrun{
#'         # Epidemic Volatility Index (EVI) Explained:
#' 	       vignette('EVI', package='EVI')
#' 	       
#'	       # For information on how to cite EVI:
#'	       citation('EVI')
#'	    }
#'
#' @importFrom stats sd
#'
#' @export
#'
#' @references
#' Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021). \doi{10.1038/s41598-021-02622-3}

deviant=function(new_cases, cum = FALSE, r_a=7, r=0.2, lag_max=30){
  #source("mova.r")
  #source("medvol.r")
  #source("evi.r")
  #source("evifcut.r")
  #source("indic.r")
  #source("status.r")
  #source("rollsd.r")




  start_time = Sys.time()
  start_cases=14
  lag_1=7
  c_1=0.01
  w_s =7



  if (cum == TRUE) new_cases = c(new_cases[1], diff(new_cases))


  #calculate the moving average of new confirmed cases
  cases=mova(new_cases,r_a)

  roll=rollsd(cases[1:start_cases],lag_1)
  ev=evi(roll)
  ind=indic(ev,c_1, cases[1:start_cases])
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
    lag_s=seq(lag_1,min(lag_max,(length(case_t)-1)), 1)
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
        evicut_t=evifcut(ev_t, case_t, l, r)
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
    evicut_n=evifcut(ev_n, case_t, c_n, r)

    roll=c(roll,roll_n[i])
    ev=c(ev,ev_n[i])
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
  EVI=ev
  Cases=cases
  Index=ind



  EVI_out=as.data.frame(cbind(Days, EVI, Cases, Index, ppv, npv,
                              lag_all, c_all, se_all, sp_all))
  
  EVI_output = EVI_out
  
  EVI_output<<-(EVI_output)

  end_time = Sys.time()

  time_elapsed = end_time - start_time
  print(time_elapsed)
  return(EVI_output)

}
