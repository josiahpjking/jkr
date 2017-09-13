##this is basically an idea Chris Crockford & I came up with during a lab. It turns out it's already been done
##basically a sort of permutations/bootstrapping/monte carlo simulation test for significance of a correlation. 
##kind of nice to make, anyway.


corrfunc<-function(x,y, sig){
  samplesizex<-length(x)
  samplesizey<-length(y)
  xmean<-mean(x)
  xsd<-sd(x)
  ymean<-mean(y)
  ysd<-sd(y)
  z<-list()
  for (n in (1:10000)){
    a<-rnorm(n = samplesizex, mean=xmean, sd = xsd)
    b<-rnorm(n = samplesizey, mean=ymean, sd = ysd)
    z<-c(z,cor(a,b))
  }
  z<-as.numeric(z)
  cor<-cor(x,y)
  critical<-qnorm(sig, mean(z), sd(z))
  if (abs(cor)>abs(critical)){
    return("SIGNIFICANT!")
  } else
    return("NON-SIGNIFICANT :(")
}

control <- c(36.4, 49.2, 26.8, 32.2, 41.9, 29.8, 36.7, 39.2, 42.3, 41.9)
treated <- c(32.2, 45.2, 31.3, 27.1, 33.4, 29.0, 24.1, 38.2, 38.0, 37.2)

corrfunc(control, treated, 0.05)
