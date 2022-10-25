
#function defining number of parameters in each model
getNumParams<-function(name){ 
  if (name %in% c("M1")){
    nParam <- 5
  }
  else if (name %in% c("M2","M4")){
    nParam <- 3
  }
  else if (name %in% c("M3")){
    nParam <-4
  }
  else if (name %in% c("M5"))
    nParam <-2
}

#function for getting log likelihood
getLL <- function(x,p,n){
  p[p<=0] <- 10e-6
  p[p>=1] <- 1 - 10e-6
  (-1)*(x*log(p) + (n-x)*log(1-p))
}

#function for getting predictions
getPredictions<-function(r1,r2,r3,f1,f2){
  pHitMS <- r1+(1-r1)*f1
  pHitColl<-r2+(1-r2)*f1
  pHitSen<-r3+(1-r3)*f1
  pFA.Ms<-(1-r1)*f2
  pFA.Coll<-(1-r2)*f2
  pFA.Sen<-(1-r3)*f2
  predictions<-c(pHitMS,pHitColl,pHitSen,pFA.Ms,pFA.Coll,pFA.Sen)
  return(predictions)
}

#function for getting predictions from each model
getP<-function(name, param){
  if (name=="M1"){
    r1<-param[1]
    r2<-param[2]
    r3 <-param[3]
    f1 <- param[4]
    f2 <- param[5]
  }
  else if (name == "M2"){
    r1<-param[1]
    r2<-param[1]
    r3<-param[1]
    f1<-param[2]
    f2<-param[3]
  } 
  else if (name == "M3"){
    r1<-param[1]
    r2<-param[2]
    r3<-param[3]
    f1<-param[4]
    f2<-param[4]
  }
  else if (modelName == "M4"){
    r1<-param[1]
    r2<-param[1]
    r3<-param[2]
    f1<-param[3]
    f2<-param[3]
  }
  else if (name == "M5"){
    r1<-param[1]
    r2<-param[1]
    r3<-param[1]
    f1<-param[2]
    f2<-param[2]
  }
  return(getPredictions(r1,r2,r3,f1,f2))
}

#maximum log likelihood function
general.mle <- function(name,param,n,hit,fa){
  predictedPoints<-getP(name,param)
  # Calculate minus log-likelihood
  loglik <- getLL(x=hit[1],predictedPoints[1],n) + getLL(x=hit[2],predictedPoints[2],n) + getLL(x=hit[3],predictedPoints[3],n)+getLL(x=fa[1],predictedPoints[4],n)+getLL(x=fa[2],predictedPoints[5],n)+getLL(x=fa[3],predictedPoints[6],n)
  return(loglik)
}


# model.1 <- function(param, n, hit, fa) {
#   #r1 is param[1]
#   #r2 is param[2]
#   #r3 is param[3]
#   #f1 is param[4]
#   #f2 is param[5]
#   pHitMS <- param[1]+(1-param[1])*param[4]
#   pHitColl<-param[2]+(1-param[2])*param[4]
#   pHitSen<-param[3]+(1-param[3])*param[4]
#   pFA.Ms<-(1-param[1])*param[5]
#   pFA.Coll<-(1-param[2])*param[5]
#   pFA.Sen<-(1-param[3])*param[5]
#   
#   # Calculate minus log-likelihood
#   loglik <- (getLL(x=hit[1],pHitMS,n) + getLL(x=hit[2],pHitColl,n) + getLL(x=hit[3],pHitSen,n)+getLL(x=fa[1],pFA.Ms,n)+getLL(x=fa[2],pFA.Coll,n)+getLL(x=fa[3],pFA.Sen,n))
#   
#   #print("loglik")
#   #print(loglik)
# }
# 
# #model 2 : r1=r2=r3
# model.2 <- function(param, n, hit, fa) {
#   #r1,2,3 is param[1]
#   #f1 is param[2]
#   #f2 is param[3]
#   pHitMS <- param[1]+(1-param[1])*param[2]
#   pHitColl<-param[1]+(1-param[1])*param[2]
#   pHitSen<-param[1]+(1-param[1])*param[2]
#   pFA.Ms<-(1-param[1])*param[3]
#   pFA.Coll<-(1-param[1])*param[3]
#   pFA.Sen<-(1-param[1])*param[3]
#   
#   # Calculate minus log-likelihood
#   loglik <- (getLL(x=hit[1],pHitMS,n) + getLL(x=hit[2],pHitColl,n) + getLL(x=hit[3],pHitSen,n)+getLL(x=fa[1],pFA.Ms,n)+getLL(x=fa[2],pFA.Coll,n)+getLL(x=fa[3],pFA.Sen,n))
#   
#   #print("loglik")
#   #print(loglik)
# }