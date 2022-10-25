#################################################################
### Model selection for HW2,Q2 in Bayesian Modeling Class #######
###  Written by Taylor Mahler, 2.7.19 ############################
#################################################################

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
source("source/helper_functions.R")

#data
n.total=250 #number of trials
n.hits <- c(227,239,217) # number of hits for each age group (MS, college, Seniors)
prop.hits <- n.hits/n.total #proportion of hits for each age group 
n.fa<- c(34,42,71) #number of false alarms for each age group
prop.fa <- n.fa/n.total #proportion of false alarms for each age group
allData<-c(n.hits,n.fa)
allProps<-c(prop.hits,prop.fa)
models <- c("M1","M2","M3","M4","M5")


#for model selection
aic.summary <- vector(length=length(models))
bic.summary <- vector(length=length(models))

#for plotting; blue is MS, green is college, red is seniors
DotColors=c("blue","green","red")

#the main program
j=1 # to keep track of which model we're looking at
for (modelName in models){
  #iterate 100 times to prevent finding a local maximum
  prevLL = -10e100000  #starting log likelihood
  for (i in range(1:100)){
    param.init<-(runif(getNumParams(modelName)))
    mle_model<-optim(param.init, general.mle,name=modelName, method="L-BFGS-B", n=n.total, hit=n.hits,fa=n.fa,lower=0,upper=1)
    if (-mle_model$value > prevLL){
      #print("current LL is greater than previous")
      #print(paste0("prevLL",prevLL))
      prevLL <- -mle_model$value
      #print(paste0("new prevLL",prevLL))
      new_mle_model <- mle_model   #replace current model with model with better parameters
    }
    #keep the model with best LL
    if (exists("new_mle_model")){
      mle_model_specific<-assign(paste("mle_model", modelName, sep = "."),new_mle_model)
    }
    else{
      mle_model_specific<-assign(paste("mle_model", modelName, sep = "."),mle_model)
    }
  }
  parameters<-mle_model_specific$par
  predictions<-getP(modelName,parameters)
  r2=1-sum((allProps-predictions)^2)/sum((allProps-mean(allProps))^2)
  
  numParam=getNumParams(modelName)
  aic.summary[j]<-2*mle_model_specific$value+2*numParam
  bic.summary[j]<-2*mle_model_specific$value+numParam*log(n.total)
  
  print(paste0("#####",modelName,"#####"))
  print("parameters")
  print(parameters)
  print("-LL:")
  print(-mle_model_specific$value,5)
  print("predictions")
  print(predictions)
  print("r2")
  print(r2)
  print("AIC")
  print(aic.summary[j])
  print("BIC")
  print(bic.summary[j])
  #save the plot
  file<-paste0("plots/Plot",modelName,".png")
  png(filename=file)
  plotTitle<-paste0("MLE Results for ",modelName)
  plot(predictions[4:6],predictions[1:3],ylim=c(0,1),xlim=c(0,.6),xlab='Proportion of False Alarms',ylab='Proportion of Hits',main=plotTitle,col=DotColors,pch=16)
  points(prop.fa, prop.hits, pch=24, cex=1.5,col=DotColors)
  legend("topright",legend=c("MS","College","Seniors"),col=DotColors,lty="solid",cex=.5)
  dev.off()
  j=j+1
}

