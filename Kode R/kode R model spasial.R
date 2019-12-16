
options(scipen = 2)

## BACA DATA 

mydatap <- read.table("D:/tugas s2/TESIS..... FIGHT!!!/Kode R/data asli.txt",header=T)
head(mydatap)


names(mydatap)
attach(mydatap)

### OLS DATA ASLI

myOLS <- lm(Y ~ H + K + P + S +TG  + KP,data=mydatap)
summary(myOLS)

#######################################
############ Uji asumsi OLS ############
#######################################
res<-residuals(myOLS)
summary(res)

## Plot Residual
plot(myOLS,which=1)
plot(myOLS,which=2)

## uji VIF
library(car)
vif(myOLS)

## uji normalitas
library(nortest)
lillie.test(res)
ad.test(res)
library(stats)
shapiro.test(res)

## Uji homoskedastis dengan Breusch-Pagan
library(lmtest)
bptest(myOLS,data=mydatap, studentize="T")

## uji autokorelasi residual
library(lmtest)
dwtest(myOLS, data=mydatap)


#####################################################
############ Uji asumsi OLS TRANSFORMASI ############
####################################################

myOLS <- lm(log(Y) ~ H + K + P +  log(KP),data=mydatap)
summary(myOLS) 

## Uji Asumsi

res<-residuals(myOLS)
summary(res)

## Plot Residual
plot(myOLS,which=1)
plot(myOLS,which=2)

## uji VIF
library(car)
vif(myOLS)

## uji normalitas
library(nortest)
lillie.test(res)
ad.test(res)
library(stats)
shapiro.test(res)

## Uji homoskedastis dengan Breusch-Pagan
library(lmtest)
bptest(myOLS,data=mydatap, studentize="T")

## uji autokorelasi residual
library(lmtest)
dwtest(myOLS, data=mydatap)

#######################################################################

#  MULAI PEMODELAN SPASIAL

#######################################################################

library(spdep)
library(sp)
library(Matrix)

############################################################
#####################      SAR      ########################
############################################################
SAR <- lagsarlm(myOLS,data=mydatap,DASw2)
summary(SAR)

resSAR<-residuals(SAR)
summary(resSAR)

lillie.test(resSAR)


###########################################################
#####################     SEM    ##########################
###########################################################
SEM <- errorsarlm(myOLS,data=mydatap,DASw2)
summary(SEM)

resSEM<-residuals(SEM)
summary(resSEM)

lillie.test(resSEM)


###########################################################
#####################     SAC      ########################
############################################################
### General Spatial Model / SAC
SAC<-sacsarlm(myOLS,data=mydatap,DASw2)
summary(SAC)

resSAC<- residuals(SAC)
summary(resSAC)

lillie.test(resSAC)


###########################################################
#####################      SDM      ########################
############################################################
SDM <- lagsarlm(myOLS,data=mydatap,DASw2,type="mixed")
summary(SDM)

SDM$SSE

SDM$interval

resSDM<-residuals(SDM)
summary(resSDM)

lillie.test(resSDM)

ad.test(resSDM)

shapiro.test(resSDM)

logLik.sarlm(SDM)

bptest.sarlm(SDM)

plot(SDM$fitted.values,resSDM, xlab="fitted values", ylab="residual")

qqplot(SDM$fitted.values, resSDM)

moran.test(resSDM,DASw2)

### r2 = 1 - (SSElm/(sum((k2-mean(k2))^2))) ##
r2 = 1 - (SDM$SSE/(sum((log(Y)-mean(log(Y)))^2)))
r2

## Plot Perbandingan OLS, SDM, dan nilai observasi
plot(myOLS$fitted.values,type="l",col="green",xlim=range(-1:92),ylim=range(-2:4),ylab="nilai pengamatan dan estimasi", xlab="ID sub-DAS")
par(new=TRUE)
plot(SDM$fitted.values,type="l",col="blue",xlim=range(-1:92),ylim=range(-2:4),ylab="nilai pengamatan dan estimasi", xlab="ID sub-DAS")
par(new=TRUE)
plot(SDM$y,col="red",type="l",xlim=range(-1:92),ylim=range(-2:4),ylab="nilai pengamatan dan estimasi", xlab="ID sub-DAS")


###########################################################
##################### Durbin Error ########################
############################################################

SDEM <- errorsarlm(myOLS,data=mydatap,DASw2,etype="emixed")
summary(SDEM)

resSDEM<-residuals(SDEM)
summary(resSDEM)

SDEM$interval

# Uji homoskedastis
bptest.sarlm(SDEM)

# Uji normal
lillie.test(resSDEM)
ad.test(resSDEM)
shapiro.test(resSDEM)

logLik.sarlm(SDEM)


moran.test(resSDEM,DASw2)

## R square

r2 = 1 - (SDEM$SSE/(sum((log(Y)-mean(log(Y)))^2)))
r2

## Perbandingan OLS, SDEM, dan nilai observasi
plot(myOLS$fitted.values,type="l",col="green",xlim=range(-1:92),ylim=range(-2:4),ylab="nilai pengamatan dan estimasi", xlab="ID sub-DAS")
par(new=TRUE)
plot(SDM$y,col="red",type="l",xlim=range(-1:92),ylim=range(-2:4),ylab="nilai pengamatan dan estimasi", xlab="ID sub-DAS")
par(new=TRUE)
plot(SDEM$fitted.values,type="l",col="purple",xlim=range(-1:92),ylim=range(-2:4),ylab="nilai pengamatan dan estimasi", xlab="ID sub-DAS")


## Perbandingan SDM dan SDEM
plot(SDM$fitted.values,type="l",col="blue",xlim=range(-1:92),ylim=range(-2:4),ylab="nilai pengamatan dan estimasi", xlab="ID sub-DAS")
par(new=TRUE)
plot(SDEM$fitted.values,type="l",col="purple",xlim=range(-1:92),ylim=range(-2:4),ylab="nilai pengamatan dan estimasi", xlab="ID sub-DAS")


###########################################################
#####################      GNSM     ########################
############################################################
#### General NEsted Spatial Model
GNSM<-sacsarlm(myOLS,data=mydatap,DASw2, type="sacmixed")
summary(GNSM)

resGNSM<- residuals(GNSM)
summary(resGNSM)

lillie.test(resGNSM)
###########################################################
#####################      SLX      ########################
############################################################
##### Spatial Lag exogeneous
SLX<- lmSLX(myOLS,data=mydatap,DASw2)
summary(SLX)

resSLX<-residuals(SLX)
summary(resSLX)

lillie.test(resSLX)

##############################################
## Perbandingan Model2 spasial #############
## berdasarkan AIC

AICs<-c(AIC(myOLS), AIC(SLX),AIC(SAR),AIC(SEM),AIC(SAC), AIC(SDM), AIC(SDEM),AIC(GNSM))
plot(AICs, type="l", lwd=1.5, xaxt="n", xlab="",col="red")
axis(1, at=1:7,labels=F) #6= number of models
labels<-c("OLS", "SLX", "SAR","SEM","SAC", "SDM","SDEM","GNM")
text(1:8, par("usr")[3]-.25, srt=50, adj=1, labels=labels, xpd=T)
mtext(side=1, text="Model Specification", line=3)

## Berdasarkan log likelihood

Loglik<-c(logLik(myOLS), logLik(SLX),logLik.sarlm(SAR),logLik.sarlm(SEM),logLik.sarlm(SAC), logLik.sarlm(SDM), logLik.sarlm(SDEM),logLik.sarlm(GNSM))
plot(Loglik, type="l", lwd=1.5, xaxt="n", xlab="",col="red")
axis(1, at=1:7,labels=F) #6= number of models
labels<-c("OLS", "SLX", "SAR","SEM","SAC", "SDM","SDEM","GNM")
text(1:8, par("usr")[3]-.25, srt=50, adj=1, labels=labels, xpd=T)
mtext(side=1, text="Model Specification", line=3)





