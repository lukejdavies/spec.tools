smooth.spec=function(spec,sc) {
    temp <-loess(spec$flux ~ spec$wave, span=sc, data.frame(x=spec$wave, y=spec$flux))
    predict <- predict(temp, data.frame(x=spec$wave))
    spec$flux <- predict
    return=spec

}
