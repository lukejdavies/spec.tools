
plot.filt=function(filters, oplot=F, xunit='ang', xlim=c(1000,5000000), xlog=T, sc=1.0, cols='NA', labels=T, lab.cex=1.5, lab.sc=0.9) {

    c<-299792458
    if (filters[1]=='all') {filters<-c('FUV', 'NUV', 'u', 'g', 'r' ,'i', 'z', 'Z', 'Y', 'J', 'H', 'K', 'W1', 'W2', 'W3', 'W4', 100, 160, 250, 350, 450)}
    if (xunit=='ang') {xtit<-'Wavelength, Ang'}
    if (xunit=='hz') {xtit<-'Frequency, Hz'}
    if (xunit=='m') {xtit<-'Wavelength, m'}
    if (xunit=='micron') {xtit<-'Wavelength, m'}
    if (xunit=='nm') {xtit<-'Wavelength, nm'}
    filter_names<-c('FUV', 'NUV', 'u', 'g', 'r' ,'i', 'z', 'Z', 'Y', 'J', 'H', 'K', 'W1', 'W2', 'W3', 'W4', 100, 160, 250, 350, 450)
    col_filters<-c('magenta', 'violet', 'cyan','skyblue', 'blue', 'limegreen' ,'darkgreen', 'red', 'gold', 'green', 'brown', 'purple', 'orange', 'magenta', 'violet', 'navy', 'limegreen', 'red', 'skyblue', 'grey', 'indianred2')
    if (cols[1]=='NA') {cols<-col_filters[which(filter_names %in% filters)]}


    for (i in 1:length(filters)) {
        filter=getfilt(filters[i])[,2:3]
        filter[,2]<-(filter[,2]/max(filter[,2]))*sc
        if (xunit=='hz') {filter[,1]<- c/(filter[,1]/10.^10)}
        if (xunit=='m') {filter[,1]<- (filter[,1]/10.^10)}
        if (xunit=='micron') {filter[,1]<- (filter[,1]/10.^4)}
        if (xunit=='nm') {filter[,1]<- (filter[,1]/10.^1)}

        if (cols[1]=='NA') {
            if (oplot==F & i==1) {magplot(filter,col=rainbow(length(filters))[i], xlim=xlim, xlab=xtit, ylab='Transmission', log='x', type='l', ylim=c(0,1))}
            else{lines(filter,col=rainbow(length(filters))[i])}
            if (labels==T) {text(median(filter[which(filter[,2]>(max(filter[,2])*0.02)),1]), sc*lab.sc, filters[i], col=rainbow(length(filters))[i], cex=lab.cex)}
        }
        if (cols[1]!='NA') {
            if (oplot==F & i==1) {magplot(filter,col=cols[i], xlim=xlim, xlab=xtit, ylab='Transmission', log='x', type='l', ylim=c(0,1))}
            else{lines(filter,col=cols[i])}
            if (labels==T) {text(median(filter[which(filter[,2]>(max(filter[,2])*0.02)),1]),sc*lab.sc, filters[i], col=cols[i], cex=lab.cex)}
        }

        
    }
}
