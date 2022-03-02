#Automated Holdout Sampling Script

start.time=proc.time()
runif(1)
percentholdout = 0.1  #sets percent to hold out each time

#number of holdout samples to run
numHoldouts = 50

#load the data
inputdata<-read.table('Beijing_2011_18.csv',header=TRUE)
attach(inputdata)

#Helper Function for holdout sampling
#Define this function in the environment before running the holdout sample
#Just highlight and run script to define in environment
{
  y = c()
  for(i in 1:length)
  {
    if(runif(1,0,1)<=percent)
    {
      y[i] = 1
    }
    else
    {
      y[i] = 0
    }
  }
  y
}

#Load the library for the regression method being used
library(mgcv)

#Define variables for the holdout script

#Data to predict from
data = inputdata
#Data to predict
actual = data$Breaks

#initialize vector of mean squared errors
vecMSE2 = matrix(data=NA,ncol=7,nrow=numHoldouts)
vecMAE2 = matrix(data=NA,ncol=7,nrow=numHoldouts)
vecME2 = matrix(data=NA,ncol=7,nrow=numHoldouts)
totalerror=matrix(data=NA,ncol=7,nrow=numHoldouts)
totalpercenterror = matrix(data=NA,ncol=7,nrow=numHoldouts)

#for loop of holdouts
for(k in 1:numHoldouts)
{
  #generate random string
  y = RandomString(percentholdout,nrow(data))
  
  #Create holdout sample for this iteration
  tmp_data = cbind(data,y)
  tmp_actual = cbind(actual,y)
  holdout=subset(tmp_data,y==1)[,1:length(data)]
  holdout_actual=subset(tmp_actual,y==1)[,1]
  leftover=subset(tmp_data,y==0)[,1:length(data)]
  
  sizeholdout<-dim(holdout)[1]
  sizeleftover<-dim(leftover)[1]
  
  #Fit Models to leftover data
  
  ############TGAM#######################################################################################################
  attach(leftover)
  
  #Fit models
  gam1<-gam(Breaks~s(Breaks_tot)+month+s(silt_loam)+s(ULC)+s(GL)+s(CL)+s(ML)+s(GSL)+s(ROC)+s(SLS)+s(C)+s(TF)+s(soils)+s(ubrthents)+s(Water)+s(plus_18)+s(white)+s(BLA)+s(AAN)+s(Asian)+HIP+s(Other)+s(Multi)+s(Latino)+s(HUPA)+s(OPA)+s(OP)+s(VPA)+s(VP)+s(AA)+s(maxT)+s(minT)+s(daily_precip)+greater_32+straddle_32,data=leftover,na.action=na.omit)
  
  lm1<-lm(Breaks~Breaks_tot+month+silt_loam+ULC+GL+CL+ML+GSL+ROC+SLS+C+TF+soils+ubrthents+Water+plus_18+white+BLA+AAN+Asian+HIP+Other+Multi+Latino+HUPA+OPA+OP+VPA+VP+AA+maxT+minT+daily_precip+greater_32+less_32+straddle_32,data=leftover)
  
  x<-cbind(leftover[,5:41])
  
  
  #Do prediction for this holdout
  detach(leftover)
  attach(holdout)
  
  gam.predict<-c()#matrix(data=NA,ncol=1,nrow=dim(holdout)[1])
  lm.predict<-c()#matrix(data=NA,ncol=1,nrow=dim(holdout)[1])
  
  for (j in 1:dim(holdout)[1]){
    gam.predict[j]<-predict(gam1,newdata=holdout[j,],type='response')
    lm.predict[j]<-predict(lm1,newdata=holdout[j,])
  }
  
  vecMSE2[k,4]<-(1/sizeholdout)*sum((gam.predict-holdout_actual)^2)
  vecMAE2[k,4]<-(1/sizeholdout)*sum(abs(gam.predict-holdout_actual))
  vecME2[k,4]<-(1/sizeholdout)*sum(gam.predict-holdout_actual)
  vecMSE2[k,5]<-(1/sizeholdout)*sum((lm.predict-holdout_actual)^2)
  vecMAE2[k,5]<-(1/sizeholdout)*sum(abs(lm.predict-holdout_actual))
  vecME2[k,5]<-(1/sizeholdout)*sum(lm.predict-holdout_actual)
  
  detach(holdout)
  
} 

detach(inputdata)

save.image('')

end.start=proc.time()-start.time
