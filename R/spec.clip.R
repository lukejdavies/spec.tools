spec.clip=function(spec, sigclip) {
    cont <- fit.cont(spec, 4)
    ex_spec <- extract.cont(spec,4)
    sig <- sd(ex_spec$flux, na.rm=T)
    spec$flux[which(abs(ex_spec$flux) > sigclip*sig)]<-cont$flux[which(abs(ex_spec$flux) > sigclip*sig)]
    return(spec)
}

