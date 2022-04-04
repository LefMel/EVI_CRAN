mova=function(cases, int=7){
  ncases=rep(NA, length(cases))
  for (i in 1:length(cases)){
    ncases[i]=mean(cases[((i+1)-min(int,i)):i])
  }
  return(ncases)
}
#cases=mova(c(2,2,1,4),7)
