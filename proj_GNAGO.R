#Auteur: GNAGO YANNICK
#Projet d'inf�rence statistique M1 SEP

   ####Les tirages d'échantillons et les diagrammes####
## Loi binomiale
N=1000
m=10
p=0.4
Bin = rbinom(N,m,p)
fa=table(Bin)# distribution des effectifs
fr=prop.table(fa)# distribution des fréquences
par(mfrow=c(2,2))
plot(ecdf(Bin),lwd=3,main="Courbe cumulative loi B(m,p) simulée",col="blue")
# Diagramme à batons
plot(fr,main="Diagramme à batons de la loi B(m,p) simulée", col=rainbow(10),
     lwd=3,xlab="0,...,m", ylab="fréquences relatives")

#fréquences cumulées théoriques
x=seq(0,m,by=1)
fct=pbinom(x,m,p)

#probabilités théoriques
frt=dbinom(x,m,p)
plot(x,fct,type="S",lwd=3,main="Courbe cumulative théorique B(m,p)",col="blue")
# Diagramme à batons théorique
plot(x,frt,type="h",main="Diagramme à batons théorique de la loi B(m,p)", col=rainbow(10),
     lwd=3,xlab="0,...,m", ylab="fréquences relatives")

## Loi de Poisson
N=500
lambda=2
Poi = rpois(N,lambda) # distribution des effectifs
fap=table(Poi) # distribution des fréquences
frp=prop.table(fap) # distribution des fréquences
par(mfrow=c(2,2))
# courbe cumulative (fonction en escalier)
plot(ecdf(Poi),lwd=3,main="Courbe cumulative loi de Poisson P(lambda)",col="blue")
# Diagramme à batons
plot(frp,main="Diagramme à batons de la loi de Poisson P(lambda)", col=rainbow(10),
     lwd=3,xlab="1,...,m", ylab="fréquences relatives")
#fréquences cumulées théoriques
mm=10*lambda
x=seq(0,mm,by=1)
fctp=ppois(x,lambda)
#probabilités théoriques
frtp=dpois(x,lambda)
plot(x,fctp,type="S",lwd=3,main="Courbe cumulative théorique P(lambda)",col="blue")
# Diagramme à batons théorique
plot(x,frtp,type="h",main="Diagramme à batons théorique de la loi P(lambda)", col=rainbow(10),
     lwd=3,xlab="0,...,m", ylab="fréquences relatives")

## Loi exponentielle
N=500
lambda=2
Exp=rexp(N,lambda)
summary(Exp)#R?sum? de la série Nor
#histograme
par(mfrow=c(1,3))
hist(Exp,freq=FALSE,main="Histogramme de donnees issues de E(lambda)",col="yellow",nclass=20)
#courbe de la densit? de la loi exponentielle
x=seq(0,lambda+10,by=0.1)
yExp=dexp(x,lambda)
lines(x,yExp,type="l",col="red",lwd=3)
#courbe de la fonction de r?partition empirique
plot(ecdf(Exp),main="courbe de la fonction de repartition empirique",col="blue",lwd=3)
#courbe de la fonction de r?partition th?orique
FrtExp=pexp(x,lambda)
plot(x,FrtExp,main="courbe de la fonction de repartition theorique",type="l",col="green",lwd=3)

## Loi Normale
N=1000
mu=1
sd=2
Nor=rnorm(N,mu,sd)

summary(Nor)#R?sum? de la s?rie Nor
#histograme
par(mfrow=c(1,3))
hist(Nor,freq=FALSE,main="Histogramme de donnees issues de N(mu,sd)",col="yellow",nclass=20,ylim=c(0,0.5))
#courbe de la densit? de la loi normale
x=seq(mu-6,mu+6,by=0.1)
yNor=dnorm(x,mu,sd)
lines(x,yNor,type="l",col="red",lwd=3)
#courbe de la fonction de r?partition empirique
plot(ecdf(Nor),main="courbe de la fonction de repartition empirique",col="blue",lwd=3)
#courbe de la fonction de r?partition th?orique
FrtNor=pnorm(x,mu,sd)
plot(x,FrtNor,main="courbe de la fonction de repartition theorique",type="l",col="green",lwd=3)

               #### Les lois des grands nombres ####
##Loi binomiale
N=1000
m=10
p=0.4
nb=1:N
moy=rep(0,times=N)
Bin=rbinom(N,m,p)
moy=cumsum(Bin)/nb
esp=rep(m*p,times=N)
#par(mfrow=c(2,2))
plot(nb,moy,type="l",col="blue",lwd=3,main="LGN pour des données de B(m,p)")
lines(nb,esp,type="l",col="red",lwd=3)
legend("topright", legend=c("esperance","moyenne empirique"),col=c("red","blue"), lty=1:2, cex=0.7)

##Loi de Poisson
N=500
lambda=2
nb=1:N
moyp=rep(0,times=N)
Poi=rpois(N,lambda)
moyp=cumsum(Poi)/nb
espp=rep(lambda,times=N)
plot(nb,moyp,type="l",col="blue",lwd=3,main="LGN pour des données de P(lambda)")
lines(nb,espp,type="l",col="red",lwd=3)
legend("topright", legend=c("esperance","moyenne empirique"),col=c("red","blue"), lty=1:2, cex=0.7)

##Loi exponentielle
N=500
lambda=1.5
nb=1:N
moyp=rep(0,times=N)
Exp=rexp(N,lambda)
moyp=cumsum(Exp)/nb
espp=rep(lambda,times=N)
plot(nb,moyp,type="l",col="blue",lwd=3,main="LGN pour des données de E(lambda)")
lines(nb,espp,type="l",col="red",lwd=3)
legend("topright", legend=c("esperance","moyenne empirique"),col=c("red","blue"), lty=1:2, cex=0.7)

##Loi normale
N=1000
mu=1
sd=2
nb=1:N
moyn=rep(0,times=N)
Nor=rnorm(N,mu,sd)
moyn=cumsum(Nor)/nb
espn=rep(mu,times=N)
plot(nb,moyn,type="l",col="blue",lwd=3,main="LGN pour des données de N(mu,sd)")
lines(nb,espn,type="l",col="red",lwd=3)
legend("topright", legend=c("esperance","moyenne empirique"),col=c("red","blue"), lty=1:2, cex=0.7)

              #### théoreme centrale limite ####
##Loi binomiale
N=1000 #taille de l'echantillon
mm=2000 #
m=10     #mu
p=0.4   #p
moy=rep(0,times=mm) #initialisé moy en 0
for(i in 1:mm){ Bin=rbinom(N,m,p) #Xn=Sn / n
moy[i]=sqrt(N)*(mean(Bin)-m*p)/sqrt(m*p*(1-p))
}
par(mfrow=c(1,1))
hist(moy,freq=FALSE,nclass=20,col="yellow",main="TCL avec des donnees binomiales")
x=seq(-4,4,by=0.1)
y=dnorm(x,0,1)
lines(x,y,type="l",col="red",lwd=3)
#legend('topright',legend=paste(c("N=","mm=","m=","p="),c(N,mm,m,p)))

##Loi de poisson
N=1000
lambda=1.5
mm=2000 #
moyp=rep(0,times=N)
for(i in 1:mm){ Poi=rpois(N,lambda)
moyp[i]=sqrt(N)*(mean(Poi)-lambda)/sqrt(lambda)
}
hist(moyp,freq=FALSE,nclass=20,col="lightblue",main="TCL avec des donnees de Poisson")
xp=seq(-4,4,by=0.1)
yp=dnorm(x,0,1)
lines(xp,yp,type="l",col="red",lwd=3)
#legend(x='topright',legend=paste(c("N=","mm=","lbd="),c(N,mm,lambda)))

## Loi de exponentielle
N=1000
lambda=1.5
mm=2000 #
moyp=rep(0,times=N)
for(i in 1:mm){ expo=rexp(N,lambda)
moyp[i]=sqrt(N)*(mean(expo)-(1/lambda))/sqrt(1/(lambda)^2)
}
hist(moyp,freq=FALSE,nclass=20,col="green",main="TCL avec des donnees de exponentielle")
xp=seq(-4,4,by=0.1)
yp=dnorm(x,0,1)
lines(xp,yp,type="l",col="red",lwd=3)
#legend(x='topright',legend=paste(c("N=","mm=","lbd="),c(N,mm,lambda)))

## Loi normale
N=1000
mm=2000 
mu=1
sd=1
nb=1:N
moyn=rep(0,times=N)
for(i in 1:mm){ Nor=rnorm(N,mu,sd)
moyn[i]=sqrt(N)*(mean(Nor)-mu)/sqrt(sd)
}
hist(moyn,freq=FALSE,nclass=16,col="pink",main="TCL avec des donnees normales")
xn=seq(-4,4,by=0.1)
yn=dnorm(x,0,1)
lines(xn,yn,type="l",col="red",lwd=3)
#legend(x='topright',legend=paste(c("N=","mm=","mu=","sd="),c(N,mm,mu,sd)))

       ####Estimation ponctuelles des prarametres ####
##loi sans biais(binomiale)
N=5000
mm=500
m=10
p=0.4
moyb=rep(0,times=mm)
varb=rep(0,times=mm)
for(i in 1:mm){
  Bin=rbinom(N,m,p)
  moyb[i]=mean(Bin)
  varb[i]=var(Bin)
}
taille=1:mm
esp=rep(m*p,times=mm)
vart=rep(m*p*(1-p),times=mm)
par(mfrow=c(1,2))
plot(taille,moyb,main="Estimation de E(X)", col="blue")
lines(taille,esp,type="l",col="red", cex=0.7)
plot(taille,varb,main="Estimation de Var(X)", col="blue")
lines(taille,vart,type="l",col="red", cex=0.7)

##loi biaisée (Loi normale)
N=5000
mm=500
mu=-2
sd=1.2
moyn=rep(0,times=mm)
varn=rep(0,times=mm)
for(i in 1:mm){
  Nor=rnorm(N,mu,sd)
  moyn[i]=mean(Nor)
  varn[i]=var(Nor)
}
abs=1:mm
espn=rep(mu,times=mm)
vartn=rep(sd^2,times=mm)
par(mfrow=c(1,2))
plot(abs,moyn,main="Estimation de E(X)", col="blue")
lines(abs,espn,type="l",col="red",cex=0.7)
plot(abs,varn,main="Estimation de Var(X)", col="blue")
lines(abs,vartn,type="l",col="red",cex=0.7)

   
         #### Les tests ####
##test sur le paramètre mu d'une loi normale de variance connue
N=20
alpha=0.05
mu0=1
sd=1.2
mm=1000
alphaest=0
ta=qnorm(1-alpha/2,0,1) #H0:"mu=mu0" contre H1:"mu#mu0"
for(i in 1:mm){
  Nor=rnorm(N,mu0,sd)
  Xb=mean(Nor)
  if(abs(sqrt(N)*(Xb-mu0)/sd)>ta){
    alphaest=alphaest+1
  }
}
alphaest=alphaest/mm
print(alphaest)
# fonction puissance
mu=seq(mu0-1.5,mu0+1.5,by=0.1)
nn=length(mu)
beta=rep(0,nn)
ta=qnorm(1-alpha/2,0,1)
for(i in 1:nn){
  for(j in 1:mm){
    Nor=rnorm(N,mu[i],sd)
    Xb=mean(Nor)
    if(abs(sqrt(N)*(Xb-mu0)/sd)>ta){
      beta[i]=beta[i]+1
    }
  }
  beta[i]=beta[i]/mm
}
seuil=rep(alpha,times=nn)
plot(mu,beta,type="l",main="fonction puissance du test sur la moyenne, sigma connue",col="red")
lines(mu,seuil,type="l",col="blue")
legend(1.5,0.98,legend=c("seuil","fonction puissance"),col=c("blue","red"),lwd=2.5,cex=0.8)

##Test bilateral sur le paramètre mu d'une loi normale de variance inconnue
N=20
alpha=0.05
mu0=1
sd=1.2
nm=100
mm=1000
alphaest=0
rep(0,times=nm)
ta=qt(1-alpha/2,N-1) #H0:"mu=mu0" contre H1:"mu#mu0"
for(j in 1:nm){
  for(i in 1:mm){
    Nor=rnorm(N,mu0,sd)
    Xb=mean(Nor)
    if(abs(sqrt(N)*(Xb-mu0)/sqrt(var(Nor)))>ta){
      alphaest[j]=alphaest[j]+1
    }
  }
  alphaest[j]=alphaest[j]/mm
}
alph=rep(alpha,times=nm)
NN=1:nm
plot(NN,alphaest,type="l",col="blue")
lines(NN,alph,type="l",col="red")
#fonction puissance
mu0=1
mu=seq(mu0-1.5,mu0+1.5,by=0.1)
nn=length(mu)
N=20
alpha=0.05
sd=1.2
mm=500
beta=rep(0,times=nn)
ta=qnorm(1-alpha/2,0,1)
for(i in 1:nn){
  for(j in 1:mm){
    Nor=rnorm(N,mu[i],sd)
    Xb=mean(Nor)
    if(abs(sqrt(N)*(Xb-mu0)/sd)>ta){
      beta[i]=beta[i]+1
    }
  }
  beta[i]=beta[i]/mm
}
seuil=rep(alpha,times=nn)
plot(mu,beta,type="l",main="foction puissance du test sur la moyenne, sigma connue",col="red")
lines(mu,seuil,type="l",col="blue")
legend(1.5,0.98,legend=c("seuil","fonction puissance"),col=c("blue","red"),lwd=2.5,cex=0.8)

##Test d'égalité des moyennes(une loi de Poisson et une loi exponentielle)
N1=10
N2=30
alpha=0.05
lambda1=2
lambda2=seq(lambda1-1.9, lambda1+2, by=0.1)
mm=length(lambda2)
nn=1000
beta=rep(0, times=mm)
ta=qnorm(1-alpha/2, 0, 1) #H0:"mu=mu0" contre H1:"mu#mu0"
for(i in 1:mm){
  for(j in 1:nn){
    Poi=rpois(N1, lambda1)
    Exp=rexp(N2, 1/lambda2[i])
    Xb1=mean(Poi)
    Xb2=mean(Exp)
    if(abs((Xb1-Xb2)/sqrt(var(Poi)/N1+var(Exp)/N2))>ta){
      beta[i]=beta[i]+1
    }
  }
  beta[i]=beta[i]/nn
}
seuil=rep(alpha, times=mm)
x11()
plot(lambda2, beta, type="l", main="fonction puissance du test sur l'égalité des moyennes", col="red")
lines(lambda2, seuil, type="l", col="green")
legend(1.05, 0.98, legend=c("seuil", "fonction puissance"), col=c("green", "red"), xjust = 0.8, lwd=2.5, cex=0.8)
legend(.68,0.77,legend=paste("n=",nn), col="black", xjust = 0.8)
legend(.68,0.57,legend=paste("N1=",N1), col="black", xjust = 0.8)
legend(.68,0.37,legend=paste("N2=",N2), col="black", xjust = 0.8)

##Test unilateral d'une loi normale de variance inconnue H0:"mu<=mu0"
mu=seq(mu0-1.5,mu0+1.5,by=0.1)
nn=length(mu)
N=100
alpha=0.05
mu0=2
sd=1.2
mm=1000
beta=rep(0,nn)
ta=qt(1-alpha,N-1)
for(i in 1:nn){
  for(j in 1:mm){
    Nor=rnorm(N,mu[i],sd)
    Xb=mean(Nor)
    Varn=var(Nor)
    if(sqrt(N)*(Xb-mu0)/sqrt(Varn)>ta){
      beta[i]=beta[i]+1
    }
  }
  beta[i]=beta[i]/mm
}
seuil=rep(alpha,times=nn)
plot(mu,beta,type="l",main="fonction puissance du test sur la moyenne, sigma inconnue",col="purple")
lines(mu,seuil,type="l",col="green")
legend(0.5,0.98,legend=c("seuil","fonction puissance"),col=c("green","purple"),lwd=2.5,cex=0.8)

         #### Regions de confiance ####

## intevalle de confiance pour mu=E(X), de la loi nomale(la variance est connue)
N=20
mu=1.5
sd=2
alpha=0.05
mm=0
nm=200
alphaest=rep(0,times=nm)
for(j in 1:nm){
  mm=100*j
  alphaest1=0
  for(i in 1:mm){
    Nor=rnorm(N,mu,sd)
    if(abs(mean(Nor)-mu)>qnorm(1-alpha/2)*sd/sqrt(N)){
      alphaest1=alphaest1+1}
  }
  alphaest[j]=alphaest1/mm
}
x=seq(100,100*nm,by=100)
AlphaT=rep(0.05,times=nm)
plot(x,alphaest,type="l",col="black",main="Variation d'un echantillon suivant une loi normale de variance connue")
lines(x,AlphaT,type="l",col="red")
# on remarque les variations sont centrées autour de alpha(ligne rouge)

## intevalle de confiance pour mu=E(X), de la loi nomale(variance est inconnue)
N=20
mu=1.5
sd=2
alpha=0.05
mm=0
nm=200
alphaest_T=rep(0,times=nm)
for(j in 1:nm){
  mm=100*j
  alphaest1=0
  for(i in 1:mm){
    Nor=rnorm(N,mu,sd)
    if(abs(mean(Nor)-mu)>qt(1-alpha/2,N-1)*sqrt(var(Nor)/N)){
      alphaest1=alphaest1+1}
  }
  alphaest_T[j]=alphaest1/mm
}
x=seq(100,100*nm,by=100)
AlphaT=rep(0.05,times=nm)
plot(x,alphaest_T,type="l",col="black",main="Variation d'un echantillon suivant une loi normale de variance inconnue") 
lines(x,AlphaT,type="l",col="red")
# on remarque les variations sont centrées autour de alpha(ligne rouge)

## Intevalle de confiance pour la variance
N=20
mu=1.5
sd=2
alpha=0.05
mm=0
nm=200
alphaest=rep(0,times=nm)
for(j in 1:nm){
  mm=100*j
  alphaest1=0
  for(i in 1:mm){
    Nor=rnorm(N,mu,sd)
    if(abs(mean(Nor)-mu)>qnorm(1-alpha/2)*sd/sqrt(N)){
      alphaest1=alphaest1+1}
  }
  alphaest[j]=alphaest1/mm
}
x=seq(100,100*nm,by=100)
AlphaT=rep(0.05,times=nm)
plot(x,alphaest,type="l",col="black",main="Variation d'un echantillon suivant une loi normale de variance connue")
lines(x,AlphaT,type="l",col="red")
# on remarque les variations sont centrées autour de alpha(ligne rouge)

##Région de confiance pour le vecteur (mu sigma2) de la loi normale

N=100
alpha=0.05
mu1=1
sd=1.2
Nor=rnorm(N,mu1,sd)
b=mean(Nor)
mu=seq(b-3,b+3,by=0.1)
nn=length(mu)
sigma1=rep(0,nn)
sigma2=rep(0,nn)
t1=qchisq(alpha/2,N-1)
t2=qchisq(1-alpha/2,N-1)
MN=rep(mean(Nor),N)
for(i in 1:nn){
  a=mu[i]
  sigma1[i]=(sum((Nor-MN)*(Nor-MN))+N*(mean(Nor)-a)^2)/t1
  sigma2[i]=(sum((Nor-MN)*(Nor-MN))+N*(mean(Nor)-a)^2)/t2
}
plot(mu,sigma2,type="l",col="blue",main="Région de confiance pour (mu, sigma^2) dans le plan")
polygon(mu,sigma2,border=TRUE,col="lightgreen")
lines(mu,sigma1,type="l",col="red")
polygon(mu,sigma1,border=FALSE,col="white")

     #### Etude globale (regresion Multiple) ####
    ##Cherchons le meilleur modele qui ajuste au mieux la variable mpg dans notre data
library(ISLR)
library(lmtest)
data<-Auto
?Auto
View(data)
str(data) #detail du dataframe
summary(data) #information sur les variables
reg1<-lm(formula = mpg~., data) #ajustement du modele avec toutes les variables
summary(reg1) 

#test de student de la non-significative
summary(reg1)$coefficients
vect.pvalues.Student<-summary(reg1)$coefficients[,"Pr(>|t|)"] #extraction de colonnes des pvalue
sort(vect.pvalues.Student)

#Mise à jour
reg1<-update(reg1,mpg~ weight + acceleration  + horsepower+ year+ cylinders+ displacement)
summary(reg1) 
#Mise à jour
reg1<-update(reg1,mpg~ weight + year+horsepower)
summary(reg1) 
#meilleur modele
reg<-reg1

      ## vérifier la normalité des résidus ####
residus <- reg$residuals
hist(residus, freq = FALSE, ylim = c(0,0.48), 
     main = "Histogramme des erreurs")
curve(dnorm(x, mean = mean(residus), sd = sd(residus)), 
      col = 2, lty = 2, lwd = 2, add = TRUE)
#les résidus suivent bien une loi normale

      ## verification de l'Autocorrelations des erreurs
acf(residus, ag.max = NULL, type = c("Autocorrelations des erreurs"),
    plot = TRUE, na.action = na.fail, demean = TRUE)
acf(residus)
plot(reg)

      ##Tester la non-corrÃ©lation (d'ordre 1) des erreurs : test de Durbin-Watson
dwtest(reg, alternative = c("two.sided"))

      ##Test d'homoscedasticitÃ© de Breusch-Pagan
require(lmtest)
bptest(reg, studentize = FALSE)
      
      ##Test de Shapiro-Wilk pour tester l'hypothése de normalité du terme d'erreur
shapiro.test( reg$residuals) 

       ##normal Q-Q plot
plot(reg, 2)
      ## l'evaluation des hypotheses
library(performance)
check_model(reg)
