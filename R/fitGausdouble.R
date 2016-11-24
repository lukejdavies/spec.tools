fitGausdouble = function(x,y,mu1,sig1,scale1,mu2,sig2,scale2){

        f = function(p){
            d = p[3]*dnorm(x,mean=p[1],sd=p[2])+p[6]*dnorm(x,mean=p[4],sd=p[5])
            sum((d-y)^2)
        }

        optim(c(mu1,sig1,scale1,mu2,sig2,scale2),f)
    }
