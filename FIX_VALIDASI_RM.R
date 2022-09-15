###Input Data###
library(readxl)
data <- read_excel("D:/STATISTIKA/MATA KULIAH/SKRIPSI/S-J/AKI AKB/DATA/DATA MGWR/ELISA.xlsx", 
                   col_types = c("text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric"))
head(data)
attach(data)
View(data)

###Analisis KOrelasi###
library(corrplot)   
corM<-cor(data[2:9])  
corM
cor.test(AKI,AKB,method =c("pearson" )) 
cor.test(AKI,X1,method =c("pearson" )) 
cor.test(AKI,X2,method =c("pearson" )) 
cor.test(AKI,X3,method =c("pearson" )) 
cor.test(AKI,X4,method =c("pearson" )) 
cor.test(AKI,X5,method =c("pearson" ))
cor.test(AKB,X1,method =c("pearson" )) 
cor.test(AKB,X2,method =c("pearson" )) 
cor.test(AKB,X3,method =c("pearson" )) 
cor.test(AKB,X4,method =c("pearson" )) 
cor.test(AKB,X5,method =c("pearson" ))
cor.test(X1,X2,method =c("pearson" ))
cor.test(X1,X3,method =c("pearson" ))
cor.test(X1,X4,method =c("pearson" ))
cor.test(X1,X5,method =c("pearson" ))
cor.test(X2,X3,method =c("pearson" ))
cor.test(X2,X4,method =c("pearson" ))
cor.test(X2,X5,method =c("pearson" ))
cor.test(X3,X4,method =c("pearson" ))
cor.test(X3,X5,method =c("pearson" ))
cor.test(X4,X5,method =c("pearson" ))

###Deteksi Multikolineritas###
library(MASS)
vif(lm(AKI+AKB~X1 + X2 + X3 + X4 + X5, data=data))
#Manual#
S11<-var(X1)
S11
S22<-var(X2)
S22
S33<-var (X3)
S33
S44<-var(X4)
S44
S55<-var(X5)
S55
S12<-cov(X1,X2)
S12
S13<-cov(X1,X3)
S13
S14<-cov(X1,X4)
S14
S15<-cov(X1,X5)
S15

S21<-cov(X2,X1)
S21
S23<-cov(X2,X3)
S23
S24<-cov(X2,X4)
S24
S25<-cov(X2,X5)
S25

S31<-cov(X3,X1)
S31
S32<-cov(X3,X2)
S32
S34<-cov(X3,X4)
S34
S35<-cov(X3,X5)
S35

S41<-cov(X4,X1)
S41
S42<-cov(X4,X2)
S42
S43<-cov(X4,X3)
S43
S45<-cov(X4,X5)
S45

S51<-cov(X5,X1)
S51
S52<-cov(X5,X2)
S52
S53<-cov(X5,X3)
S53
S54<-cov(X5,X4)
S54

###Untuk Y=X1 dengan X= X2 X3 X4 X5###
Syy<-S11
Syy
Syx<-c(S12,S13,S14,S15)
Syx
Sxx<-matrix(c(S22,S23,S24,S25,S32,S33,S34,S35,S42,S43,S44,S45,S52,S53,S54,S55),nrow=4,ncol=4)
Sxx
R<-Syx%*%solve(Sxx)%*%Syx/Syy
VIFX1<-1/(1-R)
VIFX1

###Untuk Y=X2 dengan X= X1 X3 X4 X5###
Syy<-S22
Syy
Syx<-c(S21,S23,S24,S25)
Syx
Sxx<-matrix(c(S11,S13,S14,S15,S31,S33,S34,S35,S41,S43,S44,S45,S51,S53,S54,S55),nrow=4,ncol=4)
Sxx
R<-Syx%*%solve(Sxx)%*%Syx/Syy
VIFX2<-1/(1-R)
VIFX2

###Untuk Y=X3 dengan X= X1 X2 X4 X5###
Syy<-S33
Syy
Syx<-c(S31,S32,S34,S35)
Syx
Sxx<-matrix(c(S11,S12,S14,S15,S21,S22,S24,S25,S41,S42,S44,S45,S51,S52,S54,S55),nrow=4,ncol=4)
Sxx
R<-Syx%*%solve(Sxx)%*%Syx/Syy
VIFX3<-1/(1-R)
VIFX3

###Untuk Y=X4 dengan X= X1 X2 X3 X5###
Syy<-S44
Syy
Syx<-c(S41,S42,S43,S45)
Syx
Sxx<-matrix(c(S11,S12,S13,S15,S21,S22,S23,S25,S31,S32,S33,S35,S51,S52,S53,S55),nrow=4,ncol=4)
Sxx
R<-Syx%*%solve(Sxx)%*%Syx/Syy
VIFX4<-1/(1-R)
VIFX4

###Untuk Y=X5 dengan X= X1 X2 X3 X4###
Syy<-S55
Syy
Syx<-c(S51,S52,S53,S54)
Syx
Sxx<-matrix(c(S11,S12,S13,S14,S21,S22,S23,S24,S31,S32,S33,S34,S41,S42,S43,S44),nrow=4,ncol=4)
Sxx
R<-Syx%*%solve(Sxx)%*%Syx/Syy
VIFX5<-1/(1-R)
VIFX5


###Persamaan Model###
z0 <- matrix(1,nrow=35)
X <- cbind(z0,X1,X2,X3,X4,X5)
head(X)
Y <-cbind(AKI,AKB)
head(Y)
XT = t(X)
XTX=XT%*%X
invXTX=solve(XTX)
XTY=XT%*%Y
Beta=invXTX%*%XTY
Beta

data=matrix(c(AKI,AKB,X1,X2,X3,X4,X5),ncol=7,nrow=35)
data=data.frame(data)
colnames(data)=c("AKI","AKB","X1","X2","X3","X4","X5")
model <- lm(cbind(AKI, AKB) ~ X1 + X2 + X3 + X4 + X5, data =data)
summary(model)
coef(model)

###Uji Signifikansi Parameter###
#Uji Serentak#
library(car)
lh.out <- linearHypothesis(model, hypothesis.matrix = c( "X1 = 0","X2 = 0","X3= 0","X4 = 0","X5=0")) 
lh.out

E <- lh.out$SSPE 
H <- lh.out$SSPH
det(E)/det(E + H)

X
Y
YBAR=colMeans(Y)
YBAR=matrix(c(YBAR),ncol=1,nrow=2)
Beta

n=35

E=t(Y)%*%Y-t(Beta)%*%(t(X)%*%Y)
detE=det(E)
EH=(t(Y)%*%Y)-(n*(YBAR%*%t(YBAR)))
detEH=det(EH)
WilksLambda=detE/detEH
WilksLambda

#Uji Parsial#
X1T=t(X1)
X2T=t(X2)
X3T=t(X3)
X4T=t(X4)
X5T=t(X5)

X1TX1=X1T%*%X1
X2TX2=X2T%*%X2
X3TX3=X3T%*%X3
X4TX4=X4T%*%X4
X5TX5=X5T%*%X5

invX1TX1=solve(X1TX1)
invX2TX2=solve(X2TX2)
invX3TX3=solve(X3TX3)
invX4TX4=solve(X4TX4)
invX5TX5=solve(X5TX5)

X1TY=X1T%*%Y
X2TY=X2T%*%Y
X3TY=X3T%*%Y
X4TY=X4T%*%Y
X5TY=X5T%*%Y

Beta1=invX1TX1%*%X1TY
Beta2=invX2TX2%*%X2TY
Beta3=invX3TX3%*%X3TY
Beta4=invX4TX4%*%X4TY
Beta5=invX5TX5%*%X5TY

EH1=(t(Y)%*%Y)-(t(Beta1)%*%(t(X1)%*%Y))
EH2=(t(Y)%*%Y)-(t(Beta2)%*%(t(X2)%*%Y))
EH3=(t(Y)%*%Y)-(t(Beta3)%*%(t(X3)%*%Y))
EH4=(t(Y)%*%Y)-(t(Beta4)%*%(t(X4)%*%Y))
EH5=(t(Y)%*%Y)-(t(Beta5)%*%(t(X5)%*%Y))

W1=det(E)/det(EH1)
W1
W2=det(E)/det(EH2)
W2
W3=det(E)/det(EH3)
W3
W4=det(E)/det(EH4)
W4
W5=det(E)/det(EH5)
W5

###ANALISIS RESIDUAL###
prediksi=predict(model)
Y1=as.matrix(data$AKI)
Y2=as.matrix(data$AKB)
residu=Y-prediksi

#a. Uji residual Identik (Breusch-Pangan)
library(lmtest)
bptest(lm(Y~X1+X2+X3+X4+X5, data=data))

#b. Uji residual Independen (Bartlett Sphericity)
variabel=as.factor(rep(c("AKI","AKB","X1","X2","X3","X4","X5"),each=))
index=c(AKI,AKB,X1,X2,X3,X4,X5)
data2=data.frame(variabel,index)
bartlett.test(index~variabel,data2)

#c. UJI residual berdistribusi multivariat normal 
y<-as.matrix(residu) #vektor residual
z<-t(y)
mu<-colMeans(y)
n<-nrow(y)
p<-ncol(y)
cov<-cov(y)
d<-sort(mahalanobis(y,mu,cov))
d
j<-qchisq(ppoints(n),df=p)
qqplot(j,d,main="QQ-Plot",ylab="Jarak Mahalanobis")
abline(0,1)

###Pemilihan model terbaik###
library(pracma)
I  <- eye(nrow(data))
M  <- X%*%solve(t(X)%*%X)%*%t(X)
SSPE <- t(Y) %*%(I-M)%*%Y
DF2.Mult  <- n-p
sigma.SSPE <- SSPE/DF2.Mult
sigma.SSPE
sigma.d   <- det(sigma.SSPE)
sigma.d
AICC<-(35*(log(sigma.d) +2)+2*(5+2)+(1/2*(2*(2+1))*(35/(35-2-1))))
AICC

