cenwave=function(filt){
  cenwave<-c(1:length(filt))
  for (i in 1:length(filt)){
    filt2=getfilt(filt[i])[,2:3]  
    wave=filt2[,1]
    flux=filt2[,2]
    Ptot=sum(flux)
    cenwave[i]=(1/Ptot)*sum(flux*wave)
  }
  return(cenwave)
}
