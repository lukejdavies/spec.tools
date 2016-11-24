xunit.spec=function(spec, xunit='hz') {
    c<-299792458
    xunit1 <- as.character(spec$xunit[1])
    xunit2 <- xunit
     if (xunit1=='ang') {
         if (xunit2=='hz') {
             spec$wave<-c/(spec$wave/(10.^10))
             spec$xunit <-'hz' 
         }
         if (xunit2=='micron') {
             spec$wave<-(spec$wave/(10.^4))
             spec$xunit <-'micron' 
         }
         if (xunit2=='m') {
             spec$wave<-(spec$wave/(10.^10))
             spec$xunit <-'m' 
         }
         if (xunit2=='nm') {
             spec$wave<-(spec$wave/(10.^1))
             spec$xunit <-'nm' 
         }
         
     }
    if (xunit1=='hz') {
         if (xunit2=='ang') {
             spec$wave<-c/(spec$wave/(10.^10))
             spec$xunit <-'ang' 
         }
         if (xunit2=='micron') {
             spec$wave<-c/(spec$wave/(10.^6))
             spec$xunit <-'micron' 
         }
         if (xunit2=='m') {
             spec$wave<-c/(spec$wave)
             spec$xunit <-'m' 
         }
         if (xunit2=='nm') {
             spec$wave<-c/(spec$wave/(10.^9))
             spec$xunit <-'nm' 
         }
         
     }

         if (xunit1=='micron') {
         if (xunit2=='hz') {
             spec$wave<-c/(spec$wave/(10.^6))
             spec$xunit <-'hz' 
         }
         if (xunit2=='ang') {
             spec$wave<-(spec$wave*(10.^4))
             spec$xunit <-'ang' 
         }
         if (xunit2=='m') {
             spec$wave<-(spec$wave/(10.^6))
             spec$xunit <-'m' 
         }
         if (xunit2=='nm') {
             spec$wave<-(spec$wave*(10.^3))
             spec$xunit <-'nm' 
         }
     }

    if (xunit1=='m') {
         if (xunit2=='hz') {
             spec$wave<-c/(spec$wave)
             spec$xunit <-'hz'  
         }
         if (xunit2=='micron') {
             spec$wave<-(spec$wave*(10.^6))
             spec$xunit <-'micron' 
         }
         if (xunit2=='ang') {
             spec$wave<-(spec$wave*(10.^10))
             spec$xunit <-'ang' 
         }
         if (xunit2=='nm') {
             spec$flux<-(spec$wave*(10.^9))
             spec$xunit <-'nm' 
         }
     }
    
    if (xunit1=='nm') {
         if (xunit2=='hz') {
             spec$wave<-c/(spec$wave*(10.^9))
             spec$xunit <-'hz' 
         }
         if (xunit2=='micron') {
             spec$wave<-(spec$wave/(10.^3))
             spec$xunit <-'micron' 
         }
         if (xunit2=='m') {
             spec$wave<-(spec$wave/(10.^9))
             spec$xunit <-'m' 
         }
         if (xunit2=='ang') {
             spec$wave<-(spec$wave*(10.^1))
             spec$xunit <-'ang' 
         }
     }

    return=spec
}
