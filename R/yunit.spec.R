yunit.spec=function(spec, yunit='hz') {
    c<-299792458
    yunit1 <- as.character(spec$yunit[1])
    yunit2 <- yunit
     if (yunit1=='ang') {
         if (yunit2=='hz') {
             spec$flux<-((1e-10)*spec$flux*spec$wave^2)/c
             spec$yunit <-'hz' 
         }
         if (yunit2=='Jy') {
             spec$flux<-((1e-10*spec$flux*spec$wave^2)/c)/1e-23
             spec$yunit <-'Jy' 
         }
     }
    if (yunit1=='hz') {
         if (yunit2=='ang') {
             spec$flux<-c/((1e-10)*spec$flux*spec$wave^2)
             spec$yunit <-'ang' 
         }
         if (yunit2=='Jy') {
             spec$flux<-spec$flux/1e-23
             spec$yunit <-'Jy' 
         }
     }
        if (yunit1=='Jy') {
         if (yunit2=='ang') {
             spec$flux<-(c*1e-23)/(1e-10*spec$flux*spec$wave^2)
             spec$yunit <-'ang' 
         }
         if (yunit2=='hz') {
             spec$flux<-spec$flux*1e-23
             spec$yunit <-'hz' 
         }
     }
    return=spec
}

