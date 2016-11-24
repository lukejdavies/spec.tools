
assign.line=function(spec, xlim=NA, ylim=NA) {

    xr<-c(min(spec$wave),max(spec$wave))
    yr<-c(min(spec$flux[is.finite(spec$flux)]),max(spec$flux[is.finite(spec$flux)]))

    if (is.finite(xlim[1])==T) {xr=xlim}
    if (is.finite(ylim[1])==T) {yr=ylim}

    i<-0
    while (i==0) {
        spec.plot(spec, xlim=xr, ylim=yr)
        mod <- fit.cont(spec,4)
        
            s<-readline('Enter line to be identified (enter ? for options):')
            if (s!='?') {line_u<-s}
            if (s=='?') {
                cat('Options = OIIB, OIIR, K, H, G, HB, OIIB, OIIR, Mg, Na, HA, NIIB, NIIR, SIIB, SIIR, NV, CIV,  CIII, FeII, MgII', '\n')    
                line_u<-readline('Enter line to be identified:')
            }
        
        
        all_lines <- list(load.lines(), load.lines.AGN())
        n <- unique(unlist(lapply(all_lines, names)))
        names(n) <- n
        all_lines <- lapply(n, function(ni) unlist(lapply(all_lines, `[[`, ni)))
        
        
        line_waves<-all_lines$wave_ang[which(all_lines$names==line_u)]

        cat(paste('Click on ', line_u,' line:', sep=""), '\n')
        pos<-locator(n=1)
        pos_pix <- min(which(spec$wave > pos$x))
        if (pos$y > mod$flux[pos_pix]) {pos_wave <- spec$wave[which(spec$flux[(pos_pix-5):(pos_pix+5)]==max(spec$flux[(pos_pix-5):(pos_pix+5)]))+(pos_pix-6)]}
        if (pos$y < mod$flux[pos_pix]) {pos_wave <- spec$wave[which(spec$flux[(pos_pix-5):(pos_pix+5)]==min(spec$flux[(pos_pix-5):(pos_pix+5)]))+(pos_pix-6)]}
        new_red <- (pos_wave/line_waves)-1
        spec.plot(spec, z=new_red, xlim=xr, ylim=yr)
        s<-readline(paste('** NEW REDSHIFT = ', format(new_red, nsmall=4, digits=4) ,', update redshift (y/n) or re-mark line (r):',sep=""))
        if (s=='y') {
            spec$z <- new_red
            cat('** redshift updated **', '\n')
            i<-1 
        }
        if (s=='n') {
            cat('** new redshift not used **', '\n')
            i<-1
        }
        if (s!='n' & s!='y' & s!='r') {cat('** WARNING ** COMMAND NOT RECOGNISED - REDSHIFT NOT UPDATED', '\n')}
    }
    
    return=spec
}
