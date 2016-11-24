fit.cont.order=function (spec, iter, order) {
    mod <- spec
    mod$flux <- 0
    for (i in 1:iter) {
        fit1 <- lm(spec$flux ~ poly(spec$wave, order, raw = TRUE))
        spec$flux <- spec$flux - predict(fit1, data.frame(x = spec$wave))
        mod$flux <- mod$flux + predict(fit1, data.frame(x = spec$wave))
    }
    return = mod
}

