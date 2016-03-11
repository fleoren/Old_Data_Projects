#library(R2jags)
setwd("/Users/fernanda/Dropbox/batmelon/Modelos Lineales Generalizados/")
set.seed(118716)

#leer datos
mortal<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/Bioassay.txt",header=TRUE)
n<-nrow(mortal)
#n<-1
m<-1
nef<-c(100)
xf<-c(200)

#-Defining data-
data<-list("n"=n,"ne"=mortal$No.Animales,"y"=mortal$No.Muertes,"x"=mortal$LogDose)#,"m"=m,"nef"=nef)#,"xf"=xf)

#-Defining inits-
inits<-function(){list(alpha=-10,beta=2,yf1=rep(1,n))}#,LD50=0)}

#-Selecting parameters to monitor-
parameters<-c("alpha","beta","yf1","LD50")

#########
#########INCISO B
#########
#-Running code-
#JAGS
examenb.sim<-jags(data,inits,parameters,model.file="Examen1b.txt",
              n.iter=50000,n.chains=1,n.burnin=5000)

#-Monitoring chain-

#Traza de la cadena
traceplot(examenb.sim)

#Cadena
#JAGS
out<-examenb.sim$BUGSoutput$sims.list

z<-out$alpha
par(mfrow=c(2,2))
plot(z,type="l",main='Valores de la cadena de alpha')
plot(cumsum(z)/(1:length(z)),type="l",main='Media Acumulada de alpha')
hist(z,freq=FALSE,main='Histograma de alpha')
acf(z,main='Autocorrelaciones de alpha')

z<-out$beta
par(mfrow=c(2,2))
plot(z,type="l",main='Valores de la cadena de beta')
plot(cumsum(z)/(1:length(z)),type="l",main='Media Acumulada de beta')
hist(z,freq=FALSE,main='Histograma de beta')
acf(z,main='Autocorrelaciones de beta')

#Resumen (estimadores)
#JAGS
out.sum<-examenb.sim$BUGSoutput$summary
print(out.sum)

#DIC
out.dic<-examenb.sim$DIC
print(out.dic)

#########
#########INCISO C
#########
#-Running code-
#JAGS
examenc.sim<-jags(data,inits,parameters,model.file="Examen1c.txt",
                 n.iter=50000,n.chains=1,n.burnin=5000)

#-Monitoring chain-

#Traza de la cadena
traceplot(examenc.sim)

#Cadena
#JAGS
out<-examenc.sim$BUGSoutput$sims.list

z<-out$alpha
par(mfrow=c(2,2))
plot(z,type="l",main='Valores de la cadena de alpha')
plot(cumsum(z)/(1:length(z)),type="l",main='Media Acumulada de alpha')
hist(z,freq=FALSE,main='Histograma de alpha')
acf(z,main='Autocorrelaciones de alpha')

z<-out$beta
par(mfrow=c(2,2))
plot(z,type="l",main='Valores de la cadena de beta')
plot(cumsum(z)/(1:length(z)),type="l",main='Media Acumulada de beta')
hist(z,freq=FALSE,main='Histograma de beta')
acf(z,main='Autocorrelaciones de beta')

#Resumen (estimadores)
#JAGS
out.sum<-examenc.sim$BUGSoutput$summary
print(out.sum)

#DIC
out.dic<-examenc.sim$DIC
out.dic<-examenc.sim$BUGSoutput$DIC
print(out.dic)

#########
#########INCISO D
#########
#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
or<-order(mortal$LogDose)
ymin<-min(mortal$No.Muertes,out.yf[,c(1,3,7)])
ymax<-max(mortal$No.Muertes,out.yf[,c(1,3,7)])

par(mfrow=c(1,1))
plot(mortal$LogDose,mortal$No.Muertes,ylim=c(ymin,ymax))
#Modelo 1
lines(mortal$LogDose[or],out.yf[or,1],lwd=2,col=2)
lines(mortal$LogDose[or],out.yf[or,3],lty=2,col=2)
lines(mortal$LogDose[or],out.yf[or,7],lty=2,col=2)
#Modelo 2
lines(mortal$LogDose[or],out.yf[or,1],lwd=2,col=3)
lines(mortal$LogDose[or],out.yf[or,3],lty=2,col=3)
lines(mortal$LogDose[or],out.yf[or,7],lty=2,col=3)

plot(mortal$No.Muertes,out.yf[,1])
abline(a=0,b=1)
cor(mortal$No.Muertes,out.yf[,1])

#########
#########INCISO E1
#########

inits<-function(){list(alpha=0,beta=0.1,yf1=rep(1,n))}#,LD50=0)}

#-Running code-
#JAGS
examene1.sim<-jags(data,inits,parameters,model.file="Examen1e1.txt",
                  n.iter=50000,n.chains=1,n.burnin=5000)

#-Monitoring chain-

#Traza de la cadena
traceplot(examene1.sim)

#Cadena
#JAGS
out<-examene1.sim$BUGSoutput$sims.list

z<-out$LD50[out$LD50> -1]
z2<-exp(z)
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z2,freq=FALSE,ylim=c(0,5),main="Densidad posterior de LD50")
lines(density(z2),col='red')
plot(sort(z))
acf(z)

prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

prob(z2)

z<-out$alpha
par(mfrow=c(2,2))
plot(z,type="l",main='Valores de la cadena de alpha')
plot(cumsum(z)/(1:length(z)),type="l",main='Media Acumulada de alpha')
hist(z,freq=FALSE,main='Histograma de alpha')
acf(z,main='Autocorrelaciones de alpha')

z<-out$beta
par(mfrow=c(2,2))
plot(z,type="l",main='Valores de la cadena de beta')
plot(cumsum(z)/(1:length(z)),type="l",main='Media Acumulada de beta')
hist(z,freq=FALSE,main='Histograma de beta')
acf(z,main='Autocorrelaciones de beta')

#Resumen (estimadores)
#JAGS
out.sum<-examene1.sim$BUGSoutput$summary
print(out.sum)

#DIC
out.dic<-examene1.sim$DIC
out.dic<-examene1.sim$BUGSoutput$DIC
print(out.dic)

#########
#########INCISO E2
#########
#-Running code-
#JAGS
examene2.sim<-jags(data,inits,parameters,model.file="Examen1e2.txt",
                  n.iter=50000,n.chains=1,n.burnin=5000)

#-Monitoring chain-

#Traza de la cadena
traceplot(examene2.sim)

#Cadena
#JAGS
out<-examene2.sim$BUGSoutput$sims.list

z<-out$alpha
par(mfrow=c(2,2))
plot(z,type="l",main='Valores de la cadena de alpha')
plot(cumsum(z)/(1:length(z)),type="l",main='Media Acumulada de alpha')
hist(z,freq=FALSE,main='Histograma de alpha')
acf(z,main='Autocorrelaciones de alpha')

z<-out$beta
par(mfrow=c(2,2))
plot(z,type="l",main='Valores de la cadena de beta')
plot(cumsum(z)/(1:length(z)),type="l",main='Media Acumulada de beta')
hist(z,freq=FALSE,main='Histograma de beta')
acf(z,main='Autocorrelaciones de beta')

#Resumen (estimadores)
#JAGS
out.sum<-examene2.sim$BUGSoutput$summary
print(out.sum)

#DIC
out.dic<-examene2.sim$DIC
out.dic<-examene2.sim$BUGSoutput$DIC
print(out.dic)
