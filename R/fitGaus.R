fitGaus = function(x,y,mu,sig,scale){
        f = function(p){
            d = p[3]*dnorm(x,mean=p[1],sd=p[2])
            sum((d-y)^2)
        }

        fitP<-optim(c(mu,sig,scale),f)
        return=fitP
    }
