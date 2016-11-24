
extract.cont=function(spec, iter=4) {
for (i in 1:iter) {
        fit1 <- lm(spec$flux ~ poly(spec$wave, 4, raw=TRUE))
        spec$flux <- spec$flux-predict(fit1, data.frame(x=spec$wave))
    }
    return=spec
}
