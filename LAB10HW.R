#Q1
pmfbinom<-function(r,n,p){
  cum<-0
  for (i in 0:r) {
    cum<-dbinom(i,n,p)+cum
  }
  return(cum)
}


pmfbinom(3,10,0.6)
pbinom(3,10,0.6)

#Q2


simpower<-function(n,delta,sd,sig.level){
  N<-10000
  counter<-0
  for (i in 1:N){
    sample<-rnorm(n,delta,sd)
    test<-t.test(sample,mu=0,conf.level = (1-sig.level))
    if(test$p.value<sig.level){
      counter <- counter + 1
    }
  }
  
  return(counter/N)
}

simpower(30,0.5,1,0.05)
power.t.test(30,0.5,type='one.sample')

