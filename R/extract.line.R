extract.line=function(spec,line, width, z=NA) {
    if (is.finite(z)==F) {z<-spec$z[1]}
    

    if (is.numeric(line)==F) {
        if (is.finite(z)==F) {z<-readline('**** PLEASE PROVIDE A REDSHIFT FOR THIS SPECTRUM *****')}
        lines<-load.lines()
        line_names <- as.character(lines$name)
        if (line=='all') {line<-line_names}
        
        sel<-(which((line_names %in% line)==T))
        if (spec$xunit[1]=='ang') {line_x <- as.numeric(lines$wave_ang[sel])*(1+z)}
        if (spec$xunit[1]=='hz') {line_x <- as.numeric(lines$freq_hz[sel])/(1+z)}
        if (spec$xunit[1]=='micron') {line_x <- as.numeric(lines$wave_micron[sel])*(1+z)}
        if (spec$xunit[1]=='m') {line_x <- as.numeric(lines$wave_m[sel])*(1+z)}
        if (spec$xunit[1]=='nm') {line_x <- as.numeric(lines$wave_nm[sel])*(1+z)}        
    }
    if (is.numeric(line)==T) {
        line_x <- line
    }
    mod<-fit.cont(spec, 4)
    
    for (i in 1:length(line_x)) {
      tmp<-line_x[i]
      spec$flux[which(spec$wave > (tmp-(width/2.0)) & spec$wave < (tmp+(width/2.0)))] <- mod$flux[which(spec$wave > (tmp-(width/2.0)) & spec$wave < (tmp+(width/2.0)))]
    }

    return=spec
    
}
