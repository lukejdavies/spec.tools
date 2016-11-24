add.line=function(spec,line,sigma, EW, z=NA) {
    
    if (is.finite(z)==F) {z<-spec$z[1]}
    

    if (is.character(line)==T) {
        if (is.finite(z)==F) {z<-readline('**** PLEASE PROVIDE A REDSHIFT FOR THIS SPECTRUM *****')}
        lines<-load.lines()
        line_names <- as.character(lines$name)

        if (line=='all') {
            line<-line_names
            EW2 <- c(1:length(line))
            EW2[] <- EW
            EW<-EW2
        }
        
        
        sel<-(which((line_names %in% line)==T))
                
        if (spec$xunit[1]=='ang') {line_x <- as.numeric(lines$wave_ang[sel])*(1+z)}
        if (spec$xunit[1]=='hz') {line_x <- as.numeric(lines$freq_hz[sel])/(1+z)}
        if (spec$xunit[1]=='micron') {line_x <- as.numeric(lines$wave_micron[sel])*(1+z)}
        if (spec$xunit[1]=='m') {line_x <- as.numeric(lines$wave_m[sel])*(1+z)}
        if (spec$xunit[1]=='nm') {line_x <- as.numeric(lines$wave_nm[sel])*(1+z)}        
    }
    if (is.character(line)==F) {
        line_x <- line
    }
    
    mod<-fit.cont(spec, 4)
    for (i in 1:length(line_x)) {

	    sigma_ang<-sigma
        
        gauss <- dnorm(spec$wave, mean = line_x[i], sd = sigma_ang)*EW[i]*mod$flux[min(which(spec$wave > line_x[i]))]
     
        if (length(which(is.finite(gauss)==T))>0) {spec$flux <-spec$flux+gauss}
    }

    return=spec
    
}
