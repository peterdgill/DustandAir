#We have data about how often office occupants are in samples.
#Install packages
install.packages('readxl')
#Assume K occupants. How probable is it to obtain all K over n samples?
#Place data file into a folder and set the workign directory so the program can find it
#setwd("[pathname"])
fn = "Data.xlsx"

pdf(paste0("ProbCalcs.pdf"))

#Define PRIOR Same for all experiments
alpha=3 #assumed hyper para m 
curve(dbeta(x,alpha,alpha),main="Prior distr",xlab="observation probability",ylab="Density")
mtext(paste0("hyperparam alpha=",alpha))

library(readxl)
sheets = excel_sheets(fn)
for(sheet in sheets) {
  print(sheet)
  sheet = "DUST"
  dat = read_xlsx(fn,sheet=sheet)
  
  #colnames(dat)
  
  for(o in 1:nrow(dat)) {
   # o = 2
    office = dat[o,1][[1]]
    #obtain data
    #n = sum(dat[o,3:4]) #number of total trials
     n = dat[o, 3][[1]]
    X = as.integer(unlist(dat[o,4+1:7])) #number of observations
    suppressWarnings({X = X[!is.na(X)] }) #remove not expected
    K = length(X) #number occupants
    
    #helpfunctions
    phi_n = function(phi,n) prod(1 - (1-phi)^n) #Target funtion
    ilogit = function(x) 1/(1+exp(-x))
    logit = function(x) log(x/(1-x))
    logLik = function(pvec) {#defined likelihood function
      val = sum(dbinom(X,n,pvec,log=TRUE))
      val = val + sum(dbeta(pvec,alpha,alpha,log=TRUE))
      return(val)
    }
    neglogLik = function(phi) return(-logLik(ilogit(phi)))
    
    #OPTIMIZE PARAM
    phatEXP = (X+alpha)/(n+2*alpha) #estimated posterior expectation (moment estimator)
    #neglogLik(logit( phatEXP))
    foo = nlm(neglogLik, logit( phatEXP) )
    #foo$minimum #31.53387
    phatMODE = ilogit(foo$estimate) #estimated mode (MLE)
    
    nvec = 1:30 #number of experiments
    prlim = 0.95
    prEXP = sapply(nvec, function(x) phi_n(phatEXP,x)) #posterior expextation
    prMODE = sapply(nvec, function(x) phi_n(phatMODE,x)) #posterior mode
    
    txt = paste0("Experiment: ",sheet,"| Office: ", office)
    yv = seq(0,1,0.05)
    xv = nvec
    plot(0,0,xlim=range(nvec),ylim=0:1,xlab="number of samples (S_r)",ylab="pr(all occupants sampled)",ty="n",main=txt,axes=FALSE)
    mtext(paste0("K=",K, " occupants"))
    axis(1,at=xv,las=2)
    axis(2,at=yv,las=2)
    abline(h=yv,col="gray")
    abline(v=xv,col="gray")
    lines(nvec,prMODE,lty=1,lwd=1.5)
    lines(nvec,prEXP,lty=2,lwd=1.5)
    abline(h=prlim,lty=2);
    
    #BAYESIAN
    M = 10000 #number of draws from posterior distr
    post_pvec = matrix(NA,nrow=M,ncol=K)
    for(k in 1:K) post_pvec[,k] = rbeta(M,X[k]+alpha, n-X[k]+alpha )
    
    PHI_n = function(PMAT,n) exp(rowSums(log(1 - (1-PMAT)^n)))
    
    pp = 0.1 #confidence interval
    for(n1 in nvec) {
      post_phi = PHI_n(post_pvec,n1)
      qq = quantile(post_phi,c(pp/2,1-pp/2))
      med = quantile(post_phi,0.5)
      lines(rep(n1,2),qq,col=2,lwd=2,lty=1)
      points(n1,med,pch=10,col=2,cex=1.5)
    }
    
    legend("bottomright",c("EXP","MODE","MEDIAN",paste0((1-pp)*100,"% coverage")),lty=c(2,1,NA,1),col=c(1,1,2,2),pch=c(NA,NA,10,NA))
    
  }
}
dev.off()

