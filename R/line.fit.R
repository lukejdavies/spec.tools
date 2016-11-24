line.fit=function(spec,linefit,z=NA, FWM=65, plot = F, col='red') {

    orig_xunit<-spec$xunit
    if (spec$xunit!='ang') {spec<-xunit.spec(spec,'ang') }

    
    if (is.finite(z)==F) {z<-spec$z[1]}


    if (is.numeric(linefit)==F) {
        if (is.finite(z)==F) {z<-readline('**** PLEASE PROVIDE A REDSHIFT OF THIS SPECTRUM *****')}


        lines <- load.lines()

        DB_line_list <- as.character(lines$names)
        DB_line_wave <- as.numeric(lines$wave_ang)*(1+z)
        DB_line_stell <- lines$stellar

        if (linefit[1]=='all') {linefit<-DB_line_list}
    
        line_names <- DB_line_list[DB_line_list %in% linefit]
        line_wave <- DB_line_wave[DB_line_list %in% linefit]
        line_stell <- DB_line_stell[DB_line_list %in% linefit]
    }
    
    if (is.numeric(linefit)==T) {
        line_names <- rep('User selected',length(linefit))
        line_wave <- linefit
        line_stell <- rep(F,length(linefit))
   }     
    

    mod<-fit.cont(spec, 4)
    spec<-extract.cont(spec, 4)
    LINE_FLUX <- line_wave
    LINE_FLUX_ERR <- line_wave
    LINE_SN <- line_wave
    LINE_EW <- line_wave
    LINE_EW_ERR <- line_wave
    LINE_SIG <- line_wave
    LINE_NAMES <- line_names
    LINE_WAVE <- line_wave
    CONT_FLUX <- line_wave
    
    for (i in 1:length(line_names)) {
        if (line_stell[i]==F) {step <- FWM/2.0}
        if (line_stell[i]==T) {step <- FWM}
                            
        cut_spec <- cbind(spec$wave[which(spec$wave > (line_wave[i]-step) & spec$wave < (line_wave[i]+step))],spec$flux[which(spec$wave > (line_wave[i]-step) & spec$wave < (line_wave[i]+step))])

        
        colnames(cut_spec) <- c('wave', 'flux')
        sc<-1

        if (length(cut_spec[,2]) > 2) {
            
            if (cut_spec[(length(cut_spec[,2])/2.0),2] < 0) {sc <- -1}
            spec2<-spec
            spec2$flux<-spec2$flux*sc

            cut_spec <- cbind(spec2$wave[which(spec2$wave > (line_wave[i]-step) & spec2$wave < (line_wave[i]+step))],spec2$flux[which(spec2$wave > (line_wave[i]-step) & spec2$wave < (line_wave[i]+step))])
            colnames(cut_spec) <- c('wave', 'flux')

            jump<-0
            if (length(cut_spec[,2]) > 2) { 
                 for (j in 1:10) {
                     if (length(cut_spec[,2]) > 0) { 
                         if (abs(which(cut_spec[,2]==max(cut_spec[,2]))-step) > 3.0) {
                             step <- step-1
                             cut_spec <- cbind(spec2$wave[which(spec2$wave > (line_wave[i]-step) & spec2$wave < (line_wave[i]+step))],spec2$flux[which(spec2$wave > (line_wave[i]-step) & spec2$wave < (line_wave[i]+step))])
                         }
                     } else {
                     jump<-1
                     }
                 }
        
                 if (jump==0) {
                     colnames(cut_spec) <- c('wave', 'flux')
                     cut_mod <- cbind(mod$wave[which(spec2$wave > (line_wave[i]-step) & spec2$wave < (line_wave[i]+step))],mod$flux[which(spec2$wave > (line_wave[i]-step) & spec2$wave < (line_wave[i]+step))]) 

        
                     sc2<-max(cut_spec[,2])        
                     cut_spec[,2]<-cut_spec[,2]/sc2

            
                     fitP<-fitGaus(cut_spec[,1], cut_spec[,2],line_wave[i],1,1.0)
                     
                     temp_a<-(fitP$par[3]*dnorm(cut_spec[,1],fitP$par[1],fitP$par[2]))*sc2
                     temp<-(fitP$par[3]*dnorm(cut_spec[,1],fitP$par[1],fitP$par[2]))*sc2*sc
                     err<-sd((cut_spec[,2]*sc2)-temp_a)/2.0
                     temp2<- ((fitP$par[3]*dnorm(cut_spec[,1],fitP$par[1],fitP$par[2]))*sc2*sc)+cut_mod[,2]

                     if (plot==T) {
                         x_sc<-cut_spec[,1]
                         c<-299792458
                         if (orig_xunit=='micron') {x_sc<-x_sc/10^4}
                         if (orig_xunit=='m') {x_sc<-x_sc/10^10}
                         if (orig_xunit=='nm') {x_sc<-x_sc/10}
                         if (orig_xunit=='hz') {x_sc<-c/(x_sc/10^10)}
                
                         lines(x_sc, temp2, col=col)
                     }
        
                     cut_spec_o1 <- cut_spec[(1:(length(cut_spec[,1])-1)),1]
                     cut_spec_o2 <- cut_spec[(2:(length(cut_spec[,1]))),1]
                     cut_spec_diff <- cut_spec_o2-cut_spec_o1
                     cut_spec_diff <- c(cut_spec_diff,cut_spec_diff[length(cut_spec_diff)])
                     LINE_FLUX[i]<- sum(temp*cut_spec_diff)
                     LINE_FLUX_ERR[i]<- (sum((temp+err)*cut_spec_diff))-sum(temp*cut_spec_diff)
                     LINE_SN[i] <- max(abs(temp))/sd(spec$flux[which(abs(spec$flux) < 3.0*sd(spec$flux))])
                     LINE_EW[i] <- LINE_FLUX[i]/mod$flux[min(which(spec$wave > line_wave[i]))]
                     LINE_EW_ERR[i] <- ((LINE_FLUX[i]+LINE_FLUX_ERR[i])/(mod$flux[min(which(spec$wave > line_wave[i]))]))- LINE_EW[i]
                     LINE_SN[i] <- abs(LINE_EW[i])/abs(LINE_EW_ERR[i])
                     LINE_SIG[i] <- fitP$par[2]
                     CONT_FLUX[i] <-median(cut_mod[,2])

                 } else{
                     LINE_FLUX[i]<-NA
                     LINE_FLUX_ERR[i]<-NA
                     LINE_SN[i]<-NA
                     LINE_EW[i]<-NA
                     LINE_EW_ERR[i]<-NA
                     LINE_SIG[i]<-NA
                     CONT_FLUX[i]<-NA
                     
                 }
                     
             } else {
                 LINE_FLUX[i]<-NA
                 LINE_FLUX_ERR[i]<-NA
                 LINE_SN[i]<-NA
                 LINE_EW[i]<-NA
                 LINE_EW_ERR[i]<-NA
                 LINE_SIG[i]<-NA
                 CONT_FLUX[i]<-NA
             }
            
        } else {
            LINE_FLUX[i]<-NA
            LINE_FLUX_ERR[i]<-NA
            LINE_SN[i]<-NA
            LINE_EW[i]<-NA
            LINE_EW_ERR[i]<-NA
            LINE_SIG[i]<-NA
            CONT_FLUX[i]<-NA
        }
    }

        results <- data.frame(LINE_NAMES,LINE_WAVE, LINE_FLUX, LINE_FLUX_ERR,LINE_SN, LINE_EW, LINE_EW_ERR, LINE_SIG, CONT_FLUX)

        return(results)
        
}
