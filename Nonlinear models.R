data=read.csv("D:\\MSc(ASA)\\Sem 2\\FTSA\\Research\\USUnemployment.csv")
data1=data
ncol(data1)
month=rep(1:12,72)
newdata_Year=c()
for(i in 1:nrow(data)){
  new=rep(data[i,1],12)
  newdata_Year=append(newdata_Year,new)
}

data_t=c()
for(i in 1:nrow(data)){
  for(j in 2:ncol(data)){
    val=data[i,j]
    data_t=append(data_t,val)
  }
}
data_main=data.frame(newdata_Year,month,data_t,"time"=c(1:length(newdata_Year)))
(diff(data_ts,1))
data_ts=ts(data_main$data_t,start=c(1948,1),frequency = 12)
plot(data_ts)
model_ARIMA=forecast::auto.arima(data_ts)
Pred=forecast::forecast(model_ARIMA,100)
plot(c(data_ts,Pred$mean),type="l")
acf(diff(data_ts))

library(MSwM)
model_lm=lm(data_t~month,data=data_main)
model_markov=msmFit(data_t~time,k=2,data = data_main,sw=c(TRUE,T,T))
summary(model_markov)
model_markov1=msmFit(data_t~time,k=3,data = data_main,sw=c(TRUE,T,T))
summary(model_markov1)

AIC(model_markov)
AIC(model_markov1)

plotProb(model_markov,which=1)
plotProb(model_markov,which=2)
plotProb(model_markov,which=3)

AIC(model_markov1)
plot(model_markov1)
plotDiag(model_markov1)
plotReg(model_markov1,regime = 1)
plotReg(model_markov1,regime = 2)
plotReg(model_markov1,regime = 3)
